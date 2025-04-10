{ pkgs, name, lib, frontendPath ? null, psDirs ? null }:

let
  appConfig = import ./app-config.nix { inherit name; };

  # Set defaults if not provided
  effectiveFrontendPath = if frontendPath != null then frontendPath else ".";
  effectivePsDirs = if psDirs != null then psDirs else
    appConfig.purescript.codeDirs;

  # Import manifest module
  manifestModule = import ./manifest.nix {
    inherit pkgs lib;
    config = {
      inherit name;
      frontendPath = effectiveFrontendPath;
      psDirs = effectivePsDirs;
    };
  };

  # Create compile-manifest script
  compile-manifest = pkgs.writeShellScriptBin "compile-manifest" ''
    set -euo pipefail

    # Project name for file naming
    PROJECT_NAME="${name}"

    # Setup paths
    PROJECT_ROOT="$(pwd)"
    SCRIPT_DIR="$PROJECT_ROOT/script"
    MANIFEST_FILE="$SCRIPT_DIR/manifest.json"
    BASE_DIR="$SCRIPT_DIR/concat_archive"
    HASH_DIR="$BASE_DIR/.hashes"
    OUTPUT_DIR="$BASE_DIR/output"
    ARCHIVE_DIR="$BASE_DIR/archive"
    mkdir -p "$OUTPUT_DIR" "$ARCHIVE_DIR" "$HASH_DIR"

    # Get current timestamp
    TIMESTAMP=$(date '+%Y%m%d_%H%M%S')

    # Function to calculate hash for a list of files
    calculate_hash() {
        local file_list="$1"
        if [ -z "$file_list" ]; then
            echo "empty"
            return
        fi
        echo "$file_list" | xargs sha256sum 2>/dev/null | sha256sum | cut -d' ' -f1
    }

    # Function to get previous hash
    get_previous_hash() {
        local file_type=$1
        local hash_file="$HASH_DIR/''${file_type}_last_hash"
        if [ -f "$hash_file" ]; then
            cat "$hash_file"
        else
            echo ""
        fi
    }

    # Function to save current hash
    save_current_hash() {
        local file_type=$1
        local current_hash=$2
        echo "$current_hash" > "$HASH_DIR/''${file_type}_last_hash"
    }

    # Function to compile PureScript project
    compile_purescript() {
        local project_dir=$1
        local temp_file=$(mktemp)

        if timeout 60 bash -c "cd '$project_dir' && spago build" > "$temp_file" 2>&1; then
            build_status=0
        else
            build_status=$?
            if [ $build_status -eq 124 ]; then
                echo "COMPILE_STATUS: error" > "$temp_file"
                echo "BUILD_OUTPUT:" >> "$temp_file"
                echo "Build process timed out after 60 seconds" >> "$temp_file"
            fi
        fi

        echo "{-"
        if [ $build_status -eq 0 ]; then
            echo "COMPILE_STATUS: true"
            echo "BUILD_OUTPUT:"
            cat "$temp_file" | head -n 20
            echo "..."
        else
            {
                echo "COMPILE_STATUS: false"
                echo "COMPILE_ERROR:"
                cat "$temp_file"
            }
        fi
        echo "-}"
        rm "$temp_file"
    }

    # Clean up PureScript code
    clean_purescript() {
        sed 's/\([ ]*\)--.*$/\1/' | \
        perl -0777 -pe 's/{-.*?-}//gs' | \
        cat -s | \
        sed 's/[[:space:]]*$//'
    }

    # Clean up Nix code
    clean_nix() {
        sed 's/\([ ]*\)#.*$/\1/' | \
        perl -0777 -pe 's!/\*[^*]*\*+(?:[^/*][^*]*\*+)*/!!gs' | \
        cat -s | \
        sed 's/[[:space:]]*$//' | \
        sed 's/\([ ]*\){[[:space:]]*}/\1{ }/'
    }

    # Clean up web files
    clean_web() {
        cat -s | \
        sed 's/[[:space:]]*$//'
    }

    # Function to get relative path
    get_relative_path() {
        local full_path=$1
        echo "''${full_path#$PROJECT_ROOT/}"
    }

    # Function to extract error files from compilation output
    extract_error_files() {
        local compile_output="$1"
        local file_type="$2"
        local error_files=""
        local temp_file=$(mktemp)

        # Write compile output to temp file for easier processing
        echo "$compile_output" > "$temp_file"

        if [ "$file_type" = "purs" ]; then
            # First pass: Look for explicit error messages
            while IFS= read -r line; do
                if [[ $line =~ \[ERROR[[:space:]].*\][[:space:]]([^:]+): ]]; then
                    local file="''${BASH_REMATCH[1]}"
                    if [[ $file == src/* ]]; then
                        file="$PROJECT_ROOT/$file"
                    fi
                    if [[ ! $error_files =~ (^|[[:space:]])$file($|[[:space:]]) ]]; then
                        error_files="$error_files $file"
                    fi
                fi
            done < "$temp_file"

            # Second pass: Look for type errors
            if [ -z "$error_files" ]; then
                while IFS= read -r line; do
                    if [[ $line =~ "Could not match type" ]]; then
                        # Look ahead for file context
                        local context=$(grep -B 5 -A 5 "Could not match type" "$temp_file" | grep -o "src/[^[:space:]]*\.purs:[0-9]*")
                        if [[ $context =~ (src/[^:]+) ]]; then
                            local file="''${BASH_REMATCH[1]}"
                            if [[ $file == src/* ]]; then
                                file="$PROJECT_ROOT/$file"
                            fi
                            if [[ ! $error_files =~ (^|[[:space:]])$file($|[[:space:]]) ]]; then
                                error_files="$error_files $file"
                            fi
                        fi
                    fi
                done < "$temp_file"
            fi
        fi

        rm "$temp_file"
        echo "$error_files"
    }

    # Function to get included files from manifest
    get_included_files() {
        local file_type="$1"
        local section=""

        case "$file_type" in
            "purs")
                section="purescript"
                ;;
            "nix")
                section="nix"
                ;;
            "web")
                section="web"
                ;;
            *)
                echo "Error: Unknown file type $file_type" >&2
                return 1
                ;;
        esac

        if [ ! -f "$MANIFEST_FILE" ]; then
            echo "Error: Manifest file not found at $MANIFEST_FILE" >&2
            echo "Run 'generate-manifest' to create it first" >&2
            return 1
        fi

        # Get the include array for the specific section using jq
        local include_files=$(${pkgs.jq}/bin/jq -r ".$section.include[]" "$MANIFEST_FILE" 2>/dev/null)
        local file_list=""

        # Convert relative paths to full paths
        while IFS= read -r rel_path; do
            if [ -n "$rel_path" ]; then
                file_list+=" $PROJECT_ROOT/$rel_path"
            fi
        done <<< "$include_files"

        echo "$file_list"
    }

    # Function to get compilation status for filename
    get_status_for_filename() {
        local file_content="$1"
        if grep -q "COMPILE_STATUS: true" <<< "$file_content"; then
            echo "_OK"
        elif grep -q "COMPILE_STATUS: false" <<< "$file_content"; then
            echo "_ERROR"
        elif grep -q "COMPILE_STATUS: error" <<< "$file_content"; then
            echo "_ERROR"
        else
            echo ""
        fi
    }

    # Function to safely move files to archive
    safe_archive() {
        local ext=$1
        local output_files=$(find "$OUTPUT_DIR" -name "*.$ext" -type f 2>/dev/null)
        if [ -n "$output_files" ]; then
            for output_file in $output_files; do
                filename=$(basename "$output_file")
                mv "$output_file" "$ARCHIVE_DIR/$filename.old"
            done
        fi
    }

    # Main concatenation function
    concatenate_files() {
        local file_type=$1
        local output_base=$2
        local clean_function=$3
        local comment_char=$4
        local compile_function=$5

        echo "Processing $file_type files..."

        # Get files from manifest
        local files=""
        local file_list=""
        files=$(get_included_files "$file_type")
        file_list="$files"

        # If no files to process, skip this type
        if [ -z "$file_list" ]; then
            echo "No $file_type files found in manifest, skipping..."
            return
        fi

        # Calculate hash of files
        local current_hash=$(calculate_hash "$file_list")
        local previous_hash=$(get_previous_hash "$file_type")

        if [ "$current_hash" = "$previous_hash" ] && [ "$current_hash" != "empty" ]; then
            echo "No changes detected in $file_type files, skipping..."
            return
        fi

        local compile_output=""
        if [ -n "$compile_function" ]; then
            compile_output=$($compile_function "$PROJECT_ROOT")
            
            # Extract files with errors for prioritization
            local error_files=$(extract_error_files "$compile_output" "$file_type")
            if [ -n "$error_files" ]; then
                echo "Found errors in some files, prioritizing them in output..."
                
                # Reorder file list to put error files first
                local new_file_list=""
                # First add all error files
                for file in $error_files; do
                    if [[ " $file_list " =~ " $file " ]]; then
                        new_file_list+="$file "
                    fi
                done
                # Then add the rest
                for file in $file_list; do
                    if [[ ! " $error_files " =~ " $file " ]] && [[ ! " $new_file_list " =~ " $file " ]]; then
                        new_file_list+="$file "
                    fi
                done
                file_list="$new_file_list"
            fi
        fi

        # Write to temp file
        local temp_file=$(mktemp)
        {
            echo "''${comment_char}"
            echo "Generated: $(date '+%Y-%m-%d %H:%M:%S')"
            echo "Hash: $current_hash"
            echo "Files from manifest: $(echo "$file_list" | wc -w)"
            echo "''${comment_char}"
            echo ""

            if [ -n "$compile_output" ]; then
                echo "$compile_output"
                echo ""
            fi

            for file in $file_list; do
                if [ -f "$file" ]; then
                    echo "''${comment_char} FILE: $(get_relative_path "$file")"
                    cat "$file" | eval "$clean_function"
                    echo "''${comment_char} END OF: $(get_relative_path "$file")"
                    echo ""
                else
                    echo "''${comment_char} WARNING: File not found: $(get_relative_path "$file")"
                    echo ""
                fi
            done
        } > "$temp_file"

        # Determine status and move to output
        local status=$(get_status_for_filename "$(cat "$temp_file")")
        local output_file="''${output_base}''${status}.$file_type"
        mv "$temp_file" "$output_file"

        save_current_hash "$file_type" "$current_hash"
        echo "Generated new $file_type file with status $status"
    }

    # Check for manifest file or generate it
    if [ ! -f "$MANIFEST_FILE" ]; then
        echo "Manifest file not found. Generating it first..."
        generate-manifest

        if [ ! -f "$MANIFEST_FILE" ]; then
            echo "Failed to generate manifest file."
            exit 1
        fi
    fi

    # Process different file types
    safe_archive "purs"
    safe_archive "nix"
    safe_archive "web"

    purs_base="''${OUTPUT_DIR}/PureScript_''${TIMESTAMP}"
    nix_base="''${OUTPUT_DIR}/Nix_''${TIMESTAMP}"
    web_base="''${OUTPUT_DIR}/Web_''${TIMESTAMP}"

    echo -e "\nProcessing files according to manifest..."

    concatenate_files "purs" "$purs_base" "clean_purescript" "--" "compile_purescript"
    concatenate_files "nix" "$nix_base" "clean_nix" "#" ""
    concatenate_files "web" "$web_base" "clean_web" "<!--" ""

    echo "Concatenation complete. Output files are in $OUTPUT_DIR"
    echo "Previous files have been moved to $ARCHIVE_DIR"
  '';

  # Create the compile-archive script
  compile-archive = pkgs.writeShellScriptBin "compile-archive" ''
    set -euo pipefail

    # Run compile-manifest first
    compile-manifest

    # Create archive directory
    PROJECT_NAME="${name}"
    PROJECT_ROOT="$(pwd)"
    ARCHIVE_DIR="$PROJECT_ROOT/script/archives"
    mkdir -p "$ARCHIVE_DIR"

    TIMESTAMP=$(date '+%Y%m%d_%H%M%S')
    ARCHIVE_NAME="$PROJECT_NAME-$TIMESTAMP.tar.gz"
    ARCHIVE_PATH="$ARCHIVE_DIR/$ARCHIVE_NAME"

    echo "Creating archive for $PROJECT_NAME at $ARCHIVE_PATH..."
    tar -czf "$ARCHIVE_PATH" -C "$PROJECT_ROOT" script/concat_archive/output

    echo "Archive created successfully."
  '';

in {
  inherit compile-manifest compile-archive;

  generate-manifest = manifestModule.generateScript;

  tools = [
    compile-manifest
    compile-archive
    manifestModule.generateScript
  ];

  debug = manifestModule.generateScript;
}
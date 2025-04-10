# PureScript-only dev scripts for type-novel
{ pkgs, name, lib, frontendPath ? ".", psDirs ? null }:

let
  appConfig = import ./app-config.nix { inherit name; };

  # Set effective PureScript directories
  effectivePsDirs = if psDirs != null then psDirs else
    map (path: builtins.replaceStrings ["./"] [""] path) appConfig.purescript.codeDirs;

  compile-manifest = pkgs.writeShellScriptBin "compile-manifest" ''
    #!/usr/bin/env bash
    set -euo pipefail
    
    # Configuration
    PROJECT_NAME="${name}"
    PS_DIRS="${lib.concatStringsSep " " effectivePsDirs}"
    
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
        
        # Attempt to build with timeout
        if timeout 60 bash -c "cd '$project_dir' && spago build" > "$temp_file" 2>&1; then
            build_status=0
        else
            build_status=$?
            # Check if it was a timeout
            if [ $build_status -eq 124 ]; then
                echo "COMPILE_STATUS: error" > "$temp_file"
                echo "BUILD_OUTPUT:" >> "$temp_file"
                echo "Build process timed out after 60 seconds" >> "$temp_file"
            fi
        fi
        
        # Format the compilation status and output
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
    
    # Function to clean PureScript content
    clean_purescript() {
        # Remove single-line comments while preserving indentation
        sed 's/\([ ]*\)--.*$/\1/' | \
        # Remove multi-line comments while preserving line structure
        perl -0777 -pe 's/{-.*?-}//gs' | \
        # Remove consecutive blank lines but keep one
        cat -s | \
        # Remove trailing whitespace while preserving indentation
        sed 's/[[:space:]]*$//'
    }
    
    # Function to clean Nix content with preserved formatting
    clean_nix() {
        # Remove single-line comments while preserving indentation
        sed 's/\([ ]*\)#.*$/\1/' | \
        # Remove multi-line comments while preserving line structure
        perl -0777 -pe 's!/\*[^*]*\*+(?:[^/*][^*]*\*+)*/!!gs' | \
        # Remove consecutive blank lines but keep one
        cat -s | \
        # Remove trailing whitespace while preserving indentation
        sed 's/[[:space:]]*$//' | \
        # Clean up empty attribute sets while preserving indentation
        sed 's/\([ ]*\){[[:space:]]*}/\1{ }/'
    }
    
    # Function to clean web files
    clean_web() {
        # Just handle standard whitespace cleanup
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
    
    # Function to concatenate files of a specific type
    concatenate_files() {
        local file_type=$1
        local output_base=$2
        local clean_function=$3
        local comment_char=$4
        local compile_function=$5

        echo "Finding $file_type files from manifest..."
        
        # Get files from manifest
        local file_list=$(get_included_files "$file_type")
        local error_files=""
        local compile_output=""
        
        # If no files to process, exit this function
        if [ -z "$file_list" ]; then
            echo "No $file_type files selected for processing."
            return
        fi
        
        echo "Found $(echo "$file_list" | wc -w) $file_type files to process."

        # Calculate current hash
        local current_hash=$(calculate_hash "$file_list")
        local previous_hash=$(get_previous_hash "$file_type")

        # Check if we need to create a new file - always regenerate on first run
        local force_regenerate=false
        if [ ! -f "$OUTPUT_DIR"/*."$file_type" ]; then
            force_regenerate=true
        fi

        if [ "$force_regenerate" = false ] && [ -n "$previous_hash" ] && [ "$current_hash" = "$previous_hash" ]; then
            echo "No changes detected in $file_type files, reusing previous content..."
            local latest_file=$(ls -t "$OUTPUT_DIR"/*."$file_type" 2>/dev/null | head -n1)
            if [ -n "$latest_file" ]; then
                local content=$(cat "$latest_file")
                local status=$(get_status_for_filename "$content")
                local output_file="''${output_base}''${status}.$file_type"
                echo "$content" > "$output_file"
                echo "Copied existing $file_type file with status $status"
                return
            fi
        fi

        # Capture compilation output if compile function is provided
        if [ -n "$compile_function" ]; then
            echo "Running compilation for $file_type..."
            compile_output=$(eval "$compile_function \"$PROJECT_ROOT\"")
            error_files=$(extract_error_files "$compile_output" "$file_type")
            
            if [ -n "$error_files" ]; then
                echo "Found $(echo "$error_files" | wc -w) files with errors."
            else
                echo "No compilation errors found."
            fi
        fi

        # Create final file list with error files first
        local final_file_list=""
        if [ -n "$error_files" ]; then
            # Add error files first (if they're in the included files)
            for error_file in $error_files; do
                if [[ $file_list =~ (^|[[:space:]])$error_file($|[[:space:]]) ]]; then
                    final_file_list="$final_file_list $error_file"
                fi
            done
        fi
        
        # Add remaining files
        for file in $file_list; do
            if [[ ! $error_files =~ (^|[[:space:]])$file($|[[:space:]]) ]] && [[ ! $final_file_list =~ (^|[[:space:]])$file($|[[:space:]]) ]]; then
                final_file_list="$final_file_list $file"
            fi
        done

        echo "Generating concatenated $file_type file..."
        
        # Generate output file
        local temp_file=$(mktemp)
        {
            if [ "$comment_char" = "--" ]; then
                echo "{-"
                echo "Generated: $(date '+%Y-%m-%d %H:%M:%S')"
                echo "Hash: $current_hash"
                echo "Files from manifest: $(echo "$file_list" | wc -w)"
                echo "-}"
            else
                echo "/*"
                echo "Generated: $(date '+%Y-%m-%d %H:%M:%S')"
                echo "Hash: $current_hash"
                echo "Files from manifest: $(echo "$file_list" | wc -w)"
                echo "*/"
            fi
            echo ""
            
            # Add compilation output if present
            if [ -n "$compile_output" ]; then
                echo "$compile_output"
                echo ""
            fi

            for file in $final_file_list; do
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

        # Get the status and create the final filename
        local status=$(get_status_for_filename "$(cat "$temp_file")")
        local output_file="''${output_base}''${status}.$file_type"
        mv "$temp_file" "$output_file"

        # Save the current hash
        save_current_hash "$file_type" "$current_hash"
        echo "Generated new $file_type file with status $status"
    }
    
    # Check if the manifest exists
    if [ ! -f "$MANIFEST_FILE" ]; then
        echo "Manifest file not found. Generating it first..."
        generate-manifest
        
        if [ ! -f "$MANIFEST_FILE" ]; then
            echo "Failed to generate manifest file."
            exit 1
        fi
    fi
    
    # Archive old files
    safe_archive "purs"
    safe_archive "nix"
    safe_archive "web"
    
    # Define base filenames
    purs_base="''${OUTPUT_DIR}/PureScript_''${TIMESTAMP}"
    nix_base="''${OUTPUT_DIR}/Nix_''${TIMESTAMP}"
    web_base="''${OUTPUT_DIR}/Web_''${TIMESTAMP}"
    
    # Process files
    echo -e "\nProcessing files according to manifest..."
    
    concatenate_files "purs" "$purs_base" "clean_purescript" "--" "compile_purescript"
    concatenate_files "nix" "$nix_base" "clean_nix" "#" ""
    concatenate_files "web" "$web_base" "clean_web" "<!--" ""
    
    echo "Concatenation complete. Output files are in $OUTPUT_DIR"
    echo "Previous files have been moved to $ARCHIVE_DIR"
  '';
  
  compile-archive = pkgs.writeShellScriptBin "compile-archive" ''
    #!/usr/bin/env bash
    set -euo pipefail
    
    # Run the compile-manifest command first
    compile-manifest
    
    # Create an archive of the output
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
}
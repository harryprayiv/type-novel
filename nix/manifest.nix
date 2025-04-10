{ lib, pkgs, config ? {} }:

let
  # Default configuration for PureScript-only project
  defaultConfig = {
    name = "type-novel"; # Default name if not provided
    projectRoot = ".";
    frontendPath = ".";
    psDirs = [ "src" "test" ];

    psConfig = {
      spagoFile = "./spago.yaml";
      extensions = [ ".purs" ];
    };
    nixConfig = {
      extensions = [ ".nix" ];
      dirs = [ "." "nix" ];
    };

    excludePatterns = [
      ".spago"
      "node_modules"
      "output"
      ".psci_modules"
    ];
  };

  # Merge default config with provided config
  cfg = lib.recursiveUpdate defaultConfig config;
  
  # Build exclude patterns
  excludePatternStr = lib.concatMapStringsSep "\\|" (p: p) cfg.excludePatterns;

  # Generate the manifest script
  generateManifestScript = pkgs.writeShellScriptBin "generate-manifest" ''
    #!/usr/bin/env bash
    set -euo pipefail

    # Use the project name consistently
    PROJECT_NAME="${cfg.name}"
    PROJECT_ROOT="$(pwd)"
    SCRIPT_DIR="$PROJECT_ROOT/script"
    MANIFEST_FILE="$SCRIPT_DIR/manifest.json"
    mkdir -p "$SCRIPT_DIR"

    echo "Generating manifest for $PROJECT_NAME..."

    # Define base directories
    FRONTEND_PATH="${cfg.frontendPath}"
    
    # Define PureScript directories
    PURESCRIPT_DIRS=()
    for dir in ${lib.concatStringsSep " " cfg.psDirs}; do
      PURESCRIPT_DIRS+=("$PROJECT_ROOT/$FRONTEND_PATH/$dir")
    done

    # Find PureScript files
    echo "Finding PureScript files..."
    PS_FILES=()

    for dir in "''${PURESCRIPT_DIRS[@]}"; do
      if [ -d "$dir" ]; then
        echo "Scanning $dir for PureScript files"
        while IFS= read -r file; do
          if [ -n "$file" ]; then
            rel_path="''${file#$PROJECT_ROOT/}"
            PS_FILES+=("$rel_path")
          fi
        done < <(find "$dir" -type f -name "*.purs" 2>/dev/null | grep -v "${excludePatternStr}" | sort)
      fi
    done

    # Find Nix files
    echo "Finding Nix files..."
    NIX_FILES=()

    # Project root nix files
    if [ -d "$PROJECT_ROOT" ]; then
      while IFS= read -r file; do
        rel_path="''${file#$PROJECT_ROOT/}"
        if [ -f "$file" ] && [[ "$file" != *"/script/concat_archive/"* ]]; then
          NIX_FILES+=("$rel_path")
        fi
      done < <(find "$PROJECT_ROOT" -maxdepth 1 -type f -name "*.nix" 2>/dev/null | sort)
    fi

    # Nix directory files
    if [ -d "$PROJECT_ROOT/nix" ]; then
      while IFS= read -r file; do
        rel_path="''${file#$PROJECT_ROOT/}"
        NIX_FILES+=("$rel_path")
      done < <(find "$PROJECT_ROOT/nix" -type f -name "*.nix" 2>/dev/null | sort)
    fi

    # Find web files
    echo "Finding web files..."
    WEB_FILES=()

    if [ -d "$PROJECT_ROOT" ]; then
      while IFS= read -r file; do
        rel_path="''${file#$PROJECT_ROOT/}"
        if [ -f "$file" ] && [[ "$file" != *"/node_modules/"* ]] && [[ "$file" != *"/output/"* ]]; then
          WEB_FILES+=("$rel_path")
        fi
      done < <(find "$PROJECT_ROOT" -maxdepth 1 -type f \( -name "*.html" -o -name "*.css" -o -name "*.js" \) 2>/dev/null | sort)
    fi

    # Create manifest JSON
    echo "{" > $MANIFEST_FILE
    echo "  \"meta\": {" >> $MANIFEST_FILE
    echo "    \"generated\": \"$(date '+%s')\","  >> $MANIFEST_FILE
    echo "    \"humanTime\": \"$(date '+%Y-%m-%d %H:%M:%S')\","  >> $MANIFEST_FILE
    echo "    \"projectRoot\": \"$PROJECT_ROOT\""  >> $MANIFEST_FILE
    echo "  },"  >> $MANIFEST_FILE

    # Add PureScript section
    echo "  \"purescript\": {"  >> $MANIFEST_FILE
    echo "    \"include\": ["  >> $MANIFEST_FILE
    if [ ''${#PS_FILES[@]} -gt 0 ]; then
      for i in "''${!PS_FILES[@]}"; do
        if [ $i -eq $((''${#PS_FILES[@]}-1)) ]; then
          echo "      \"''${PS_FILES[$i]}\""  >> $MANIFEST_FILE
        else
          echo "      \"''${PS_FILES[$i]}\","  >> $MANIFEST_FILE
        fi
      done
    fi
    echo "    ],"  >> $MANIFEST_FILE
    echo "    \"exclude\": [],"  >> $MANIFEST_FILE
    echo "    \"count\": ''${#PS_FILES[@]},"  >> $MANIFEST_FILE
    echo "    \"timestamp\": \"$(date '+%s')\""  >> $MANIFEST_FILE
    echo "  },"  >> $MANIFEST_FILE

    # Add Nix section
    echo "  \"nix\": {"  >> $MANIFEST_FILE
    echo "    \"include\": ["  >> $MANIFEST_FILE
    if [ ''${#NIX_FILES[@]} -gt 0 ]; then
      for i in "''${!NIX_FILES[@]}"; do
        if [ $i -eq $((''${#NIX_FILES[@]}-1)) ]; then
          echo "      \"''${NIX_FILES[$i]}\""  >> $MANIFEST_FILE
        else
          echo "      \"''${NIX_FILES[$i]}\","  >> $MANIFEST_FILE
        fi
      done
    fi
    echo "    ],"  >> $MANIFEST_FILE
    echo "    \"exclude\": [],"  >> $MANIFEST_FILE
    echo "    \"count\": ''${#NIX_FILES[@]},"  >> $MANIFEST_FILE
    echo "    \"timestamp\": \"$(date '+%s')\""  >> $MANIFEST_FILE
    echo "  },"  >> $MANIFEST_FILE

    # Add Web section
    echo "  \"web\": {"  >> $MANIFEST_FILE
    echo "    \"include\": ["  >> $MANIFEST_FILE
    if [ ''${#WEB_FILES[@]} -gt 0 ]; then
      for i in "''${!WEB_FILES[@]}"; do
        if [ $i -eq $((''${#WEB_FILES[@]}-1)) ]; then
          echo "      \"''${WEB_FILES[$i]}\""  >> $MANIFEST_FILE
        else
          echo "      \"''${WEB_FILES[$i]}\","  >> $MANIFEST_FILE
        fi
      done
    fi
    echo "    ],"  >> $MANIFEST_FILE
    echo "    \"exclude\": [],"  >> $MANIFEST_FILE
    echo "    \"count\": ''${#WEB_FILES[@]},"  >> $MANIFEST_FILE
    echo "    \"timestamp\": \"$(date '+%s')\""  >> $MANIFEST_FILE
    echo "  }"  >> $MANIFEST_FILE
    echo "}"  >> $MANIFEST_FILE

    # Format with jq
    ${pkgs.jq}/bin/jq . "$MANIFEST_FILE" > "$MANIFEST_FILE.tmp" && mv "$MANIFEST_FILE.tmp" "$MANIFEST_FILE"

    # Create backup
    BACKUP_TIME=$(date '+%Y%m%d_%H%M%S')
    cp "$MANIFEST_FILE" "$MANIFEST_FILE.$BACKUP_TIME"

    echo "Manifest generated at: $MANIFEST_FILE"
    echo "Backup created at: $MANIFEST_FILE.$BACKUP_TIME"
    echo "Found ''${#PS_FILES[@]} PureScript files, ''${#NIX_FILES[@]} Nix files, and ''${#WEB_FILES[@]} web files"

    echo ""
    echo "Searched directories:"
    echo "PureScript directories:"
    for dir in "''${PURESCRIPT_DIRS[@]}"; do
      if [ -d "$dir" ]; then
        echo "  - $dir (exists)"
      else
        echo "  - $dir (does not exist)"
      fi
    done
  '';

  # Manifest data structure
  manifestData = {
    meta = {
      name = cfg.name;
      projectRoot = cfg.projectRoot;
      frontendPath = cfg.frontendPath;
    };
    purescript.include = [];
    purescript.exclude = [];
    purescript.count = 0;
    nix.include = [];
    nix.exclude = [];
    nix.count = 0;
    web.include = [];
    web.exclude = [];
    web.count = 0;
  };

in {
  data = manifestData;

  json = builtins.toJSON manifestData;

  generateScript = generateManifestScript;
}
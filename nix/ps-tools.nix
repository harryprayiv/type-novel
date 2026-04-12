{ pkgs, lib ? pkgs.lib, name }:

let
  appConfig = import ./config.nix { inherit name; };
  vitePort  = toString appConfig.vite.port;

  vite-cleanup = pkgs.writeShellApplication {
    name          = "vite-cleanup";
    runtimeInputs = [ pkgs.lsof ];
    text          = ''
      VITE_PORT="${vitePort}"

      if lsof -i :"$VITE_PORT" > /dev/null 2>&1; then
        echo "Found processes on port $VITE_PORT"
        lsof -t -i :"$VITE_PORT" | while read -r pid; do
          if [ -n "$pid" ]; then
            echo "Killing process $pid"
            kill "$pid" 2>/dev/null || true
            RETRIES=0
            while kill -0 "$pid" 2>/dev/null; do
              RETRIES=$((RETRIES+1))
              if [ "$RETRIES" -eq 5 ]; then
                echo "Process $pid not responding, forcing shutdown..."
                kill -9 "$pid" 2>/dev/null || true
                break
              fi
              sleep 1
            done
          fi
        done
        if ! lsof -i :"$VITE_PORT" > /dev/null 2>&1; then
          echo "Successfully cleaned up all processes"
        else
          echo "Failed to clean up some processes"
          exit 1
        fi
      else
        echo "No processes found on port $VITE_PORT"
      fi
    '';
  };

  vite = pkgs.writeShellApplication {
    name          = "vite";
    runtimeInputs = [ pkgs.nodejs_20 pkgs.lsof ];
    text          = ''
      VITE_PORT="${vitePort}"

      cleanup_port() {
        local port="$1"
        local pids
        pids=$(lsof -t -i :"$port" 2>/dev/null)
        if [ -n "$pids" ]; then
          echo "Found processes using port $port:"
          echo "$pids" | while read -r pid; do
            echo "Killing process $pid"
            kill "$pid" 2>/dev/null || true
          done
          RETRIES=0
          while lsof -i :"$port" > /dev/null 2>&1; do
            RETRIES=$((RETRIES+1))
            if [ "$RETRIES" -eq 10 ]; then
              echo "Some processes not responding, forcing shutdown..."
              echo "$pids" | while read -r pid; do
                kill -9 "$pid" 2>/dev/null || true
              done
              break
            fi
            echo "Waiting for port to be freed... (attempt $RETRIES/10)"
            sleep 1
          done
        fi
      }

      if lsof -i :"$VITE_PORT" > /dev/null 2>&1; then
        echo "Port $VITE_PORT is in use. Attempting to clean up..."
        cleanup_port "$VITE_PORT"
      fi

      exec node_modules/.bin/vite --port "$VITE_PORT" --host --open
    '';
  };

  spago-watch = pkgs.writeShellApplication {
    name          = "spago-watch";
    runtimeInputs = [ pkgs.entr pkgs.spago-unstable ];
    text          = ''find {src,test} | entr -s "spago $*" '';
  };

  concurrent = pkgs.writeShellApplication {
    name          = "concurrent";
    runtimeInputs = [ pkgs.concurrently ];
    text          = ''
      concurrently \
        --color "auto" \
        --prefix "[{command}]" \
        --handle-input \
        --restart-tries 10 \
        "$@"
    '';
  };

  bundle = pkgs.writeShellApplication {
    name          = "bundle";
    runtimeInputs = [
      pkgs.purs
      pkgs.purs-backend-es
      pkgs.esbuild
      pkgs.nodejs_20
      pkgs.spago-unstable
    ];
    text          = ''
      set -euo pipefail

      OUT_DIR="dist"
      MINIFY=true
      MODE=es

      for arg in "$@"; do
        case "$arg" in
          --no-minify) MINIFY=false ;;
          --mode=*)    MODE="''${arg#--mode=}" ;;
          --out=*)     OUT_DIR="''${arg#--out=}" ;;
          --help)
            echo "Usage: bundle [--mode es|simple] [--no-minify] [--out <dir>]"
            echo ""
            echo "Modes:"
            echo "  es     (default) spago build -> purs-backend-es DCE -> esbuild --minify"
            echo "  simple           spago build -> entry shim     -> esbuild --minify"
            exit 0 ;;
        esac
      done

      mkdir -p "$OUT_DIR"

      echo "--- Step 1: spago build (mode: $MODE)..."
      spago build
      echo "    Done."

      if [ "$MODE" = "es" ]; then
        echo "--- Step 2: purs-backend-es bundle-app (DCE)..."
        purs-backend-es bundle-app \
          --main Main \
          --to "$OUT_DIR/bundle-pre-minify.js" \
          --no-source-maps
        PRE_BYTES=$(wc -c < "$OUT_DIR/bundle-pre-minify.js")
        echo "    Pre-minify: $PRE_BYTES bytes"
        INPUT_JS="$OUT_DIR/bundle-pre-minify.js"
      else
        if [ ! -f "output/Main/index.js" ]; then
          echo "ERROR: output/Main/index.js not found after spago build"
          exit 1
        fi
        PRE_BYTES=$(wc -c < output/Main/index.js)
        echo "    Main/index.js: $PRE_BYTES bytes"
        echo 'require("./output/Main/index.js").main()' > "$OUT_DIR/_entry.js"
        INPUT_JS="$OUT_DIR/_entry.js"
      fi

      echo "--- Step 3: esbuild..."
      if [ "$MINIFY" = "true" ]; then
        esbuild "$INPUT_JS" \
          --bundle \
          --outfile="$OUT_DIR/app.js" \
          --format=iife \
          --platform=browser \
          --minify \
          --sourcemap=external
      else
        esbuild "$INPUT_JS" \
          --bundle \
          --outfile="$OUT_DIR/app.js" \
          --format=iife \
          --platform=browser \
          --sourcemap=external
      fi

      rm -f "$OUT_DIR/bundle-pre-minify.js" "$OUT_DIR/_entry.js"

      FINAL_BYTES=$(wc -c < "$OUT_DIR/app.js")
      REDUCTION=$(( (PRE_BYTES - FINAL_BYTES) * 100 / PRE_BYTES ))
      echo "    Final: $FINAL_BYTES bytes ($REDUCTION% reduction)"
      echo ""
      echo "Output: $OUT_DIR/app.js"
      echo "        $OUT_DIR/app.js.map"
    '';
  };

  dev = pkgs.writeShellApplication {
    name          = "dev";
    runtimeInputs = [ spago-watch vite concurrent ];
    text          = ''
      concurrent "spago-watch build" vite
    '';
  };

in {
  inherit vite vite-cleanup spago-watch concurrent bundle dev;
}
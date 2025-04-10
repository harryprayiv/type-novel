{ pkgs, lib ? pkgs.lib, name, frontend ? null }:

let
  appConfig = import ./app-config.nix { inherit name; };

  vite-cleanup = pkgs.writeShellApplication {
    name = "vite-cleanup";
    runtimeInputs = with pkgs; [ lsof ];
    text = ''
      VITE_PORT=5173

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
    name = "vite";
    runtimeInputs = with pkgs; [ nodejs-slim lsof ];
    text = ''
      VITE_PORT=5173

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

      npx vite --port "$VITE_PORT" --host --open

      trap 'cleanup_port "$VITE_PORT"' EXIT
    '';
  };

  concurrent = pkgs.writeShellApplication {
    name = "concurrent";
    runtimeInputs = with pkgs; [ concurrently ];
    text = ''
      concurrently\
        --color "auto"\
        --prefix "[{command}]"\
        --handle-input\
        --restart-tries 10\
        "$@"
    '';
  };

  spago-watch = pkgs.writeShellApplication {
    name = "spago-watch";
    runtimeInputs = with pkgs; [ entr spago-unstable ];
    text = ''find {src,test} -name "*.purs" | entr -s "spago $*" '';
  };

  dev = pkgs.writeShellApplication {
    name = "dev";
    runtimeInputs = with pkgs; [
      nodejs-slim
      spago-watch
      vite
      concurrent
    ];
    text = ''
      concurrent "spago-watch build" vite
    '';
  };

  get-ip = pkgs.writeShellApplication {
    name = "get-ip";
    text = ''
      if [[ "$OSTYPE" == "darwin"* ]]; then
        IP=$(ipconfig getifaddr en0 || ipconfig getifaddr en1)
      else
        IP=$(ip -4 addr show | grep -oP '(?<=inet\s)\d+(\.\d+){3}' | grep -v '127.0.0.1' | head -n 1)
      fi
      echo "Network IP: $IP"
    '';
  };

in {
  inherit vite vite-cleanup spago-watch concurrent dev get-ip;

  buildInputs = with pkgs; [
    esbuild
    nodejs_20
    purs
    purs-tidy
    purs-backend-es
    purescript-language-server
    spago-unstable
  ];
}
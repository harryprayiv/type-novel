{ pkgs, lib ? pkgs.lib, name }:

let
  # Simple deploy script for PureScript & Vite application
  deploy = pkgs.writeShellScriptBin "deploy" ''
    #!/usr/bin/env bash
    set -euo pipefail

    echo "Building project..."
    spago build || { echo "PureScript build failed"; exit 1; }
    
    # Display tmux instructions
    echo "TMux Commands:"
    echo "-------------"
    echo "Ctrl-b d    - Detach (safe exit)"
    echo "Ctrl-b o    - Switch between panes"
    echo "Ctrl-b z    - Zoom/unzoom current pane"
    echo "Arrow keys  - Navigate panes (with Ctrl-b)"
    echo "Ctrl-b [    - Scroll mode (q to exit)"
    echo ""
    echo "Starting development server..."

    # Start with a clean slate
    tmux kill-session -t ${name} 2>/dev/null || true

    # Create a new tmux session with standard dimensions
    tmux new-session -d -s ${name} -n "Services" -x 120 -y 30
    
    # Split into two panes - top for spago watch, bottom for vite
    tmux split-window -v -t ${name}:Services.0 -p 35
    
    # Send commands to each pane
    tmux send-keys -t ${name}:Services.0 'spago build --watch' C-m
    tmux send-keys -t ${name}:Services.1 'npx vite --port 5173 --host localhost --open' C-m
    
    # Select the vite pane and attach
    tmux select-pane -t ${name}:Services.1
    
    # Attach to the session
    tmux attach-session -t ${name}
  '';

  # Build for production
  build = pkgs.writeShellScriptBin "build" ''
    #!/usr/bin/env bash
    set -euo pipefail
    
    echo "Building ${name} for production..."
    
    # Clean up existing build artifacts
    rm -rf output
    
    echo "Compiling PureScript..."
    spago build || { echo "PureScript build failed"; exit 1; }
    
    echo "Building with Vite..."
    npx vite build
    
    echo "Build complete! Output is in the output directory."
    
    # Optionally serve the build for testing
    if [ $# -gt 0 ] && [ "$1" = "--preview" ]; then
      echo "Starting preview server..."
      npx vite preview --port 4173
    fi
  '';

  # Script to stop development services
  stop = pkgs.writeShellScriptBin "stop" ''
    #!/usr/bin/env bash
    set -euo pipefail
    
    # Store our own PID so we don't kill ourselves
    OUR_PID=$$
    PARENT_PID=$PPID
    
    echo "Stopping vite..."
    VITE_PIDS=$(pgrep -f "vite" | grep -v "^$OUR_PID$" | grep -v "^$PARENT_PID$" || echo "")
    if [ -n "$VITE_PIDS" ]; then
      echo "Found vite processes: $VITE_PIDS"
      for pid in $VITE_PIDS; do
        echo "Killing vite process $pid"
        kill -9 "$pid" 2>/dev/null || true
      done
    fi
    
    # Check port 5173 is clean
    PORT_PIDS=$(lsof -i :5173 -t | grep -v "^$OUR_PID$" | grep -v "^$PARENT_PID$" 2>/dev/null || echo "")
    if [ -n "$PORT_PIDS" ]; then
      echo "Found processes using port 5173: $PORT_PIDS"
      for pid in $PORT_PIDS; do
        echo "Killing process $pid on port 5173"
        kill -9 "$pid" 2>/dev/null || true
      done
    fi

    echo "Stopping tmux session..."
    if tmux has-session -t ${name} 2>/dev/null; then
      echo "Sending interrupt signal to tmux panes..."
      for pane_index in 0 1; do
        tmux send-keys -t ${name}:Services.$pane_index C-c 2>/dev/null || true
      done
      
      sleep 1
      
      echo "Killing tmux session..."
      tmux kill-session -t ${name} 2>/dev/null || true
    fi

    # Final check for any remaining processes
    SPAGO_PIDS=$(pgrep -f "spago" | grep -v "^$OUR_PID$" | grep -v "^$PARENT_PID$" || echo "")
    if [ -n "$SPAGO_PIDS" ]; then
      echo "Finding remaining spago processes..."
      for pid in $SPAGO_PIDS; do
        echo "Killing spago process $pid"
        kill -9 "$pid" 2>/dev/null || true
      done
    fi

    echo "All services stopped."
  '';

  # Simple Vite server without tmux 
  serve = pkgs.writeShellScriptBin "serve" ''
    #!/usr/bin/env bash
    set -euo pipefail
    
    echo "Building PureScript..."
    spago build || { echo "PureScript build failed"; exit 1; }
    
    echo "Starting Vite server..."
    npx vite --port 5173 --host localhost --open
  '';

  preview = pkgs.writeShellScriptBin "preview" ''
    #!/usr/bin/env bash
    set -euo pipefail
    
    echo "Creating ${name} preview..."
    
    # Create a fresh preview directory
    rm -rf .preview
    mkdir -p .preview
    
    echo "Building PureScript..."
    spago build || { echo "PureScript build failed"; exit 1; }
    
    echo "Creating bundle..."
    # First make sure we're working with a clean state
    rm -f index.js
    
    # Bundle the app
    spago bundle || { echo "Bundling failed"; exit 1; }
    
    # Check if bundle was created in the root directory (which is typical behavior)
    if [ -f "index.js" ]; then
      echo "Found bundle in project root, copying to preview directory"
      cp index.js .preview/app.js
    else
      echo "Bundle not found in expected location. Exiting."
      exit 1
    fi
    
    # Create a minimal HTML file
    cat > .preview/index.html <<EOF
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>${name} Preview</title>
  <link rel="stylesheet" href="styles.css">
  <link rel="icon" href="data:,">
</head>
<body>
  <div id="app"></div>
  <script src="app.js"></script>
</body>
</html>
EOF
    
    # Copy or create styles
    if [ -f "app-styles.css" ]; then
      cp app-styles.css .preview/styles.css
    elif [ -f "styles.css" ]; then
      cp styles.css .preview/styles.css
    else
      # Create a minimal CSS file
      cat > .preview/styles.css <<EOF
/* Base styles */
html, body {
  margin: 0;
  padding: 0;
  height: 100%;
  font-family: system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
}
#app {
  padding: 1rem;
}
EOF
    fi
    
    echo "Starting preview server on http://localhost:8080"
    echo "Press Ctrl+C to stop"
    
    # Use Vite to serve the preview
    cd .preview
    npx vite --port 8080 --host localhost
  '';

in {
  inherit deploy build stop serve preview;
}
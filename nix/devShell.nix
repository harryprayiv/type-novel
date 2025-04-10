{ pkgs, name, lib, system ? builtins.currentSystem }:

let
  appConfig = import ./app-config.nix {
    inherit name;
  };

  psConfig = appConfig.purescript;
  viteConfig = appConfig.vite;

  psDirs = psConfig.codeDirs;

  frontendModule = import ./frontend.nix {
    inherit pkgs lib name;
    frontend = {
      inherit (viteConfig) viteport settings;
      inherit (psConfig) codeDirs spagoFile;
    };
  };

  manifestModule = import ./manifest.nix {
    inherit pkgs lib;
    config = {
      inherit name;
      frontendPath = ".";
      psDirs = psConfig.codeDirs;
    };
  };

  fileToolsModule = import ./file-tools.nix {
    inherit pkgs name lib;
    frontendPath = ".";
    psDirs = psConfig.codeDirs;
  };

  deployModule = import ./deploy.nix {
    inherit pkgs name lib;
  };

  # VSCode/VSCodium setup
  extensions = (with pkgs.vscode-extensions; [
    bbenoist.nix
    jnoortheen.nix-ide
  ]) ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    {
      publisher = "nwolverson";
      name = "language-purescript";
      version = "0.2.9";
      sha256 = "sha256-2uOwCHvnlQQM8s8n7dtvIaMgpW8ROeoUraM02rncH9o=";
    }
    {
      publisher = "nwolverson";
      name = "ide-purescript";
      version = "0.26.6";
      sha256 = "sha256-72DRp+XLPlOowkRqyCWAeU/MNUr01m39IkCHCm5zpVc=";
    }
  ];

  vscodiumWithExtensions = pkgs.vscode-with-extensions.override {
    vscode = pkgs.vscodium;
    vscodeExtensions = extensions;
  };

  workspaceModule = {
    code-workspace = pkgs.writeShellApplication {
      name = "code-workspace";
      runtimeInputs = with pkgs; [ vscodiumWithExtensions ];
      text = ''
        if [ ! -f "${name}.code-workspace" ]; then
          cat > ${name}.code-workspace <<EOF
        {
          "folders": [
            {
              "name": "Project Root",
              "path": "./"
            }
          ],
          "settings": {
            "[purescript]": {
              "editor.defaultFormatter": "nwolverson.ide-purescript",
              "editor.fontSize": 14
            },
            "purescript.addSpagoSources": true,
            "purescript.sourcePath": "src",
            "files.watcherExclude": {
              "**/output/**": true,
              "**/node_modules/**": true
            }
          },
          "extensions": {
            "recommendations": [
              "nwolverson.ide-purescript",
              "nwolverson.language-purescript"
            ]
          }
        }
        EOF
          echo "Created ${name}.code-workspace"
        fi

        codium ${name}.code-workspace
      '';
    };

    backup-project = pkgs.writeShellApplication {
      name = "backup-project";
      runtimeInputs = with pkgs; [ rsync ];
      text = ''
        rsync -va --delete --exclude-from='.gitignore' --exclude='.git/' ~/workdir/${name}/ ~/plutus/workspace/scdWs/${name}/
      '';
    };
  };

  # Common build inputs for PureScript & Vite development
  commonBuildInputs = with pkgs; [
    # PureScript tools
    esbuild
    nodejs_20
    nixpkgs-fmt
    purs
    purs-tidy
    purs-backend-es
    purescript-language-server
    spago-unstable
    entr
    concurrently

    # VSCode tooling
    vscodiumWithExtensions

    # Development scripts
    frontendModule.vite
    frontendModule.vite-cleanup
    frontendModule.spago-watch
    frontendModule.concurrent
    frontendModule.dev
    frontendModule.get-ip

    # File tools
    fileToolsModule.compile-manifest
    fileToolsModule.compile-archive
    manifestModule.generateScript

    # Deployment tools
    deployModule.deploy
    deployModule.build
    deployModule.stop
    deployModule.serve
    deployModule.preview

    # Include workspace module
    workspaceModule.code-workspace
    workspaceModule.backup-project

    # Core utilities
    coreutils
    bash
    gnused
    gnugrep
    jq
    perl
    findutils
    toilet
    lsof
    tmux
    python3  # For preview server
  ];

  nativeBuildInputs = with pkgs; [
    pkg-config
  ];

  darwinInputs = if (system == "aarch64-darwin" || system == "x86_64-darwin") then
    (with pkgs.darwin.apple_sdk.frameworks; [
      Cocoa
      CoreServices
    ])
  else [];

  devShell = pkgs.mkShell {
    inherit name;

    inherit nativeBuildInputs;

    buildInputs = commonBuildInputs ++ darwinInputs;

    shellHook = ''
      # Create necessary directories for project
      mkdir -p output
      mkdir -p "$(pwd)/script/concat_archive/output" "$(pwd)/script/concat_archive/archive" "$(pwd)/script/concat_archive/.hashes"
      
      # Create VSCode settings directory
      mkdir -p "$(pwd)/.vscode"
      
      cat > "$(pwd)/.vscode/argv.json" <<EOF
      {
        "disable-hardware-acceleration": true,
        "enable-crash-reporter": true,
        "crash-reporter-id": "4e77d7bd-2f26-4723-9757-4f86cefd7010"
      }
      EOF
      
      echo "Welcome to the ${lib.toSentenceCase name} dev environment!"
      
      echo "Available commands:"
      echo "  Frontend:"
      echo "    vite                   - Start Vite development server"
      echo "    vite-cleanup           - Clean frontend build artifacts"
      echo "    spago-watch            - Watch PureScript files for changes"
      echo "    concurrent             - Run concurrent development tasks"
      echo "    dev                    - Start all development services"
      echo "    get-ip                 - Display your network IP address"
      echo ""
      echo "  Project Management:"
      echo "    code-workspace         - Open VSCodium workspace"
      echo "    generate-manifest      - Generate project file manifest"
      echo "    compile-manifest       - Compile and concatenate project files"
      echo "    compile-archive        - Create a project archive"
      echo ""
      echo "  Deployment:"
      echo "    deploy                 - Start development with PureScript watch and Vite in tmux"
      echo "    serve                  - Run Vite server directly (no tmux)"
      echo "    build                  - Build for production (--preview to test)"
      echo "    preview                - Create a simple bundled preview" 
      echo "    stop                   - Kill all development processes"
      echo ""
      toilet ${lib.toSentenceCase name} -t --metal
    '';
  };

in {
  inherit devShell;
  inherit workspaceModule;
}
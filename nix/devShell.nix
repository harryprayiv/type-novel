{ pkgs
, lib                    ? pkgs.lib
, name
, system                 ? builtins.currentSystem
, psToolsModule
, psCommand
, purescript
, purs-tidy
, purescript-language-server
}:

let
  appConfig = import ./config.nix { inherit name; };

  extensions = (with pkgs.vscode-extensions; [
    mkhl.direnv
    bbenoist.nix
    jnoortheen.nix-ide
    gruntfuggly.todo-tree
  ]) ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    {
      publisher = "nwolverson";
      name      = "language-purescript";
      version   = "0.2.9";
      sha256    = "sha256-9LBdo6lj+hz2NsvPmMV73nCT7uk6Q/ViguiilngOsGc=";
    }
    {
      publisher = "nwolverson";
      name      = "ide-purescript";
      version   = "0.26.6";
      sha256    = "sha256-zYLAcPgvfouMQj3NJlNJA0DNeayKxQhOYNloRN2YuU8=";
    }
  ];

  vscodiumWithExtensions = pkgs.vscode-with-extensions.override {
    vscode           = pkgs.vscodium;
    vscodeExtensions = extensions;
  };

  darwinInputs =
    if (system == "aarch64-darwin" || system == "x86_64-darwin") then
      (with pkgs.darwin.apple_sdk.frameworks; [ Cocoa CoreServices ])
    else [];

  devShell = pkgs.mkShell {
    inherit name;

    buildInputs = [
      psCommand
      purescript
      purescript-language-server
      purs-tidy

      pkgs.purs-backend-es
      pkgs.spago-unstable
      pkgs.nodejs_20
      pkgs.esbuild
      pkgs.nixpkgs-fmt
      pkgs.gum
      pkgs.toilet

      psToolsModule.vite
      psToolsModule.vite-cleanup
      psToolsModule.spago-watch
      psToolsModule.concurrent
      psToolsModule.bundle
      psToolsModule.dev

      vscodiumWithExtensions
    ] ++ darwinInputs;

    nativeBuildInputs = [ pkgs.pkg-config ];

    shellHook = ''
      if [ ! -x node_modules/.bin/vite ]; then
        echo "vite not found in node_modules — running npm install..."
        npm install
      fi

      echo "Welcome to the ${lib.toSentenceCase name} dev environment!"
      echo ""
      echo "Available commands:"
      echo "  dev                          concurrent spago-watch + vite HMR"
      echo "  vite                         start Vite dev server on :${toString appConfig.vite.port}"
      echo "  vite-cleanup                 kill any process on :${toString appConfig.vite.port}"
      echo "  spago-watch [build|test]     watch src/test via entr"
      echo "  bundle                       production build (es mode, minified)"
      echo "  bundle --mode simple         simple esbuild bundle, no DCE"
      echo "  bundle --no-minify           skip minification"
      echo "  bundle --out <dir>           output directory (default: dist/)"
      echo "  purs-nix compile             compile via purs-nix"
      echo ""
      toilet ${lib.toSentenceCase name} -t --metal
    '';
  };

in {
  inherit devShell;
}
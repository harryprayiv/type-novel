# flake.nix
{
  description = "type-novel";

  inputs = {
    nixpkgs.url            = "github:nixos/nixpkgs/nixos-unstable";
    purs-nix.url           = "github:purs-nix/purs-nix";
    ps-tools.follows       = "purs-nix/ps-tools";
    purescript-overlay.url = "github:harryprayiv/purescript-overlay";
    flake-utils.url        = "github:numtide/flake-utils";
    flake-compat = {
      url   = "github:edolstra/flake-compat";
      flake = false;
    };

    purescript-hyrule = {
      url   = "github:mikesol/purescript-hyrule/a2a32e02a0d8518d906ec5fb3192261f63667338";
      flake = false;
    };
    purescript-deku = {
      url   = "github:mikesol/purescript-deku/276f48adde3d9354f61917f7e9ae2ae7b43df6b2";
      flake = false;
    };
    purescript-deku-css = {
      url   = "github:mikesol/purescript-deku/06a06a2908b2a400a0ab9224c8128aa5988e674d";
      flake = false;
    };
    purescript-dodo-printer = {
      url   = "github:natefaubion/purescript-dodo-printer";
      flake = false;
    };
    purescript-tidy = {
      url   = "github:natefaubion/purescript-tidy/v0.10.0";
      flake = false;
    };
    purescript-tidy-codegen = {
      url   = "github:natefaubion/purescript-tidy-codegen";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, purescript-overlay, purs-nix, ... }:
    let
      name = "type-novel";
    in
    flake-utils.lib.eachSystem
      [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ]
      (system:
      let
        lib = nixpkgs.lib;

        pkgs = import nixpkgs {
          inherit system;
          overlays = [ purescript-overlay.overlays.default ];
        };

        purs-nix-instance = purs-nix { inherit system; };

        inherit (inputs.ps-tools.legacyPackages.${system}.for-0_15)
          purescript purs-tidy purescript-language-server;

        ps-pkgs = purs-nix-instance.ps-pkgs;

        psDependencies = import ./nix/purs-nix.nix {
          inherit inputs purs-nix-instance ps-pkgs;
        };

        ps = purs-nix-instance.purs {
          dir          = ./.;
          dependencies = psDependencies;
          inherit purescript;
          nodejs = pkgs.nodejs_20;
        };

        psToolsModule = import ./nix/ps-tools.nix {
          inherit pkgs name lib;
        };

        devShellModule = import ./nix/devShell.nix {
          inherit pkgs name lib system
                  psToolsModule
                  purescript purs-tidy purescript-language-server;
          psCommand = ps.command { };
        };

      in {
        legacyPackages    = pkgs;
        devShells.default = devShellModule.devShell;
        devShell          = devShellModule.devShell;
      });

  nixConfig = {
    extra-experimental-features = [ "nix-command flakes" ];
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.nixos.org"
      "https://hercules-ci.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
    ];
  };
}
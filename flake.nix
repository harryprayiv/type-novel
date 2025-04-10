{
  description = "type-novel";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, purescript-overlay, ... }:
    let
      projectName = "type-novel";
    in
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"] (system: 
    let
      name = projectName;
      lib = nixpkgs.lib;
      
      overlays = [ purescript-overlay.overlays.default ];
      
      pkgs = import nixpkgs {
        inherit system overlays;
      };

      shellModule = import ./nix/devShell.nix {
        inherit pkgs name lib system;
      };

    in {
      inherit projectName;
      
      legacyPackages = pkgs;
      
      devShell = shellModule.devShell;
    });

  nixConfig = {
    extra-experimental-features = ["nix-command flakes"];
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
      "https://cache.nixos.org"
      "https://hercules-ci.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
    ];
  };
}


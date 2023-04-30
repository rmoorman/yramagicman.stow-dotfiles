{
  description = "A very basic flake";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = inputs@{ nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      base = [
        ../header.nix
        ../packages.nix
        ../services.nix
        ../users.nix
      ];
    in {

      nixosConfigurations."browncoat" = with base; nixpkgs.lib.nixosSystem {
        inherit system;
        modules = base ++ [
          ./default.nix
          ../systemd/store-scrub.nix
          ../systemd/store-status.nix
          ../systemd/store-balance.nix
        ];
      };

    };
}

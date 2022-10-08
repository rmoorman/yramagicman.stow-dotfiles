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
        ./fonts.nix
        ./header.nix
        ./packages.nix
        ./services.nix
        ./users.nix
      ];

    in {

      homeConfigurations.jonathan = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        # Specify your home configuration modules here, for example,
        # the path to your home.nix.
        modules = [ ./home.nix ];

        # Optionally use extraSpecialArgs
        # to pass through arguments to home.nix
      };

      nixosConfigurations."tightpants" =with base; nixpkgs.lib.nixosSystem {
        inherit system;
        modules = base ++ [
          ./tightpants
          ./yubikey.nix
          ./systemd/backup.nix
          ./systemd/scrub.nix
          ./systemd/balance.nix
          ./systemd/scrub-status.nix
          ./systemd/snapshot.nix
          home-manager.nixosModules.home-manager  {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.jonathan = import ./home.nix;
          }
        ];
      };

      nixosConfigurations."jayne" = with base; nixpkgs.lib.nixosSystem {
        inherit system;
        modules = base ++ [
          ./jayne
          home-manager.nixosModules.home-manager  {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.jonathan = import ./home.nix;
          }
        ];
      };

      nixosConfigurations."browncoat" = with base; nixpkgs.lib.nixosSystem {
        inherit system;
        modules = base ++ [
          ./browncoat
          ./systemd/store-scrub.nix
          ./systemd/store-status.nix
          ./systemd/store-balance.nix
        ];
      };

      nixosConfigurations."kaylee" = with base; nixpkgs.lib.nixosSystem {
        inherit system;
        modules = base ++ [
          ./kaylee
          ./yubikey.nix
          ./systemd/backup.nix
          ./systemd/scrub.nix
          ./systemd/scrub-status.nix
          ./systemd/snapshot.nix
          ./systemd/balance.nix
          home-manager.nixosModules.home-manager  {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.jonathan = import ./home.nix;
          }
        ];
      };
    };
}

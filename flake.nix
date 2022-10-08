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
        ./nixos/fonts.nix
        ./nixos/header.nix
        ./nixos/packages.nix
        ./nixos/services.nix
        ./nixos/users.nix
      ];
    in {

      homeConfigurations.jonathan = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        # Specify your home configuration modules here, for example,
        # the path to your home.nix.
        modules = [
          ./home.nix
        ];

        # Optionally use extraSpecialArgs
        # to pass through arguments to home.nix
      };

      nixosConfigurations."tightpants" =with base; nixpkgs.lib.nixosSystem {
        inherit system;
        modules = base ++ [
          ./nixos/tightpants
          ./nixos/yubikey.nix
          ./nixos/systemd/backup.nix
          ./nixos/systemd/scrub.nix
          ./nixos/systemd/balance.nix
          ./nixos/systemd/scrub-status.nix
          ./nixos/systemd/snapshot.nix
          home-manager.nixosModules.home-manager  {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.jonathan = {
              imports = [
                ./home.nix
                ./nixos/tightpants/packages.nix
              ];
            };
          }
        ];
      };

      nixosConfigurations."jayne" = with base; nixpkgs.lib.nixosSystem {
        inherit system;
        modules = base ++ [
          ./nixos/jayne
          home-manager.nixosModules.home-manager  {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.jonathan = {
              imports= [
                ./home.nix
                ./nixos/jayne/packages.nix
              ];
            };
          }
        ];
      };

      nixosConfigurations."browncoat" = with base; nixpkgs.lib.nixosSystem {
        inherit system;
        modules = base ++ [
          ./nixos/browncoat
          ./nixos/systemd/store-scrub.nix
          ./nixos/systemd/store-status.nix
          ./nixos/systemd/store-balance.nix
        ];
      };

      nixosConfigurations."kaylee" = with base; nixpkgs.lib.nixosSystem {
        inherit system;
        modules = base ++ [
          ./nixos/kaylee
          ./nixos/yubikey.nix
          ./nixos/systemd/backup.nix
          ./nixos/systemd/scrub.nix
          ./nixos/systemd/scrub-status.nix
          ./nixos/systemd/snapshot.nix
          ./nixos/systemd/balance.nix
          home-manager.nixosModules.home-manager  {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.jonathan = {
              imports = [ ./home.nix ];
            };
          }
        ];
      };
    };
}

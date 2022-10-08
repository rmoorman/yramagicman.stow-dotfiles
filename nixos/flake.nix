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
    in {

      homeConfigurations.jonathan = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

       # Specify your home configuration modules here, for example,
        # the path to your home.nix.
        modules = [ ./home.nix ];

        # Optionally use extraSpecialArgs
        # to pass through arguments to home.nix
      };

        nixosConfigurations."tightpants" = nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [
                ./tightpants/hardware-configuration.nix
                ./fonts.nix
                ./header.nix
                ./packages.nix
                ./services.nix
                ./yubikey.nix
                ./tightpants/extras.nix
                ./tightpants/network.nix
                ./users.nix
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

        nixosConfigurations."jayne" = nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [
                ./jayne/hardware-configuration.nix
                ./fonts.nix
                ./header.nix
                ./jayne/extras.nix
                ./jayne/network.nix
                ./packages.nix
                ./services.nix
                ./users.nix
                home-manager.nixosModules.home-manager  {

                    home-manager.useGlobalPkgs = true;
                    home-manager.useUserPackages = true;
                    home-manager.users.jonathan = import ./home.nix;
                }
            ];
        };

        nixosConfigurations."browncoat" = nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [
                ./browncoat/hardware-configuration.nix
                ./fonts.nix
                ./header.nix
                ./packages.nix
                ./services.nix
                ./browncoat/extras.nix
                ./browncoat/network.nix
                ./users.nix
                ./systemd/store-scrub.nix
                ./systemd/store-status.nix
                ./systemd/store-balance.nix
            ];
        };

        nixosConfigurations."kaylee" = nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [
                ./kaylee/hardware-configuration.nix
                ./fonts.nix
                ./header.nix
                ./packages.nix
                ./services.nix
                ./yubikey.nix
                ./kaylee/extras.nix
                ./kaylee/network.nix
                ./users.nix
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

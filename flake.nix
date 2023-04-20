{
  description = "A very basic flake";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";

    # emacs-overlay.url = "github:nix-community/emacs-overlay";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { nixpkgs,  home-manager, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      base = [

        # {
        #   nixpkgs.overlays = [ emacs-overlay.overlay];
        # }
        ./nixos/fonts.nix
        ./nixos/header.nix
        ./nixos/packages.nix
        ./nixos/services.nix
        ./nixos/users.nix
        # ./nixos/cachix.nix
      ];
    in {

      homeConfigurations.jonathan = home-manager.lib.homeManagerConfiguration {
        inherit emacs-overlay pkgs;


        # Specify your home configuration modules here, for example,
        # the path to your home.nix.
        modules = [

          ./home.nix
          ./nixos/not-nixos
        ];

        # Optionally use extraSpecialArgs
        # to pass through arguments to home.nix
      };

      nixosConfigurations."tightpants" =with base; nixpkgs.lib.nixosSystem {
        inherit system;
        modules = base ++ [
          ./nixos/tightpants
          ./nixos/yubikey.nix
          ./nixos/xserver.nix
          ./nixos/doas.nix
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

      nixosConfigurations."kaylee" = with base; nixpkgs.lib.nixosSystem {
        inherit system;
        modules = base ++ [
          ./nixos/kaylee
          ./nixos/yubikey.nix
          ./nixos/xserver.nix
          ./nixos/doas.nix
          ./nixos/systemd/backup.nix
          ./nixos/systemd/scrub.nix
          ./nixos/systemd/scrub-status.nix
          ./nixos/systemd/snapshot.nix
          ./nixos/systemd/balance.nix
          home-manager.nixosModules.home-manager  {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.jonathan = {
              imports = [
                ./home.nix
                ./nixos/kaylee/packages.nix
              ];
            };
          }
        ];
      };
    };
}

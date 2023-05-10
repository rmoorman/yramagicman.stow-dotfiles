{
  description = "A very basic flake";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";

    # emacs-overlay.url = "github:nix-community/emacs-overlay";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { nixpkgs, home-manager, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      base = [
        ./nixos/fonts.nix
        ./nixos/header.nix
        ./nixos/packages.nix
        ./nixos/services.nix
        ./nixos/users.nix
        ./nixos/auto-upgrade.nix
        ./nixos/doas.nix
        ./nixos/systemd
        ./nixos/xserver.nix
        ./nixos/yubikey.nix
      ];
    in {

      homeConfigurations.jonathan = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        # Specify your home configuration modules here, for example,
        # the path to your home.nix.
        modules = [
          ./home.nix
          ./nixos/not-nixos
        ];

        # Optionally use extraSpecialArgs
        # to pass through arguments to home.nix
      };


      homeConfigurations.tightpants = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        # Specify your home configuration modules here, for example,
        # the path to your home.nix.
        modules = [
          ./home.nix
          ./nixos/tightpants/packages.nix
          ./nixos/vlc.nix
        ];

        extraSpecialArgs = {
          host="tightpants";
        };
        # Optionally use extraSpecialArgs
        # to pass through arguments to home.nix
      };

      homeConfigurations.kaylee = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        # Specify your home configuration modules here, for example,
        # the path to your home.nix.
        modules = [
          ./home.nix
          ./nixos/kaylee/packages.nix
          ./nixos/vlc.nix
        ];

        extraSpecialArgs = {
          host="kaylee";
        };
        # Optionally use extraSpecialArgs
        # to pass through arguments to home.nix
      };

      nixosConfigurations.tightpants = with base; nixpkgs.lib.nixosSystem {
        inherit system;
        modules = base ++ [
          ./nixos/tightpants
        ];
      };


      nixosConfigurations."kaylee" = with base; nixpkgs.lib.nixosSystem {
        inherit system;
        modules = base ++ [
          ./nixos/kaylee
          ./nixos/virtualization.nix
        ];
      };
    };
}

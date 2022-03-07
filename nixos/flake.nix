{
    description = "A very basic flake";

    outputs = { self, nixpkgs }: {

        nixosConfigurations."tightpants" = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
                ./tightpants/hardware-configuration.nix
                ./fonts.nix
                ./header.nix
                ./packages.nix
                ./services.nix
                ./tightpants/extras.nix
                ./tightpants/network.nix
                ./users.nix
            ];
        };

        nixosConfigurations."jayne" = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
                ./jayne/hardware-configuration.nix
                ./fonts.nix
                ./header.nix
                ./packages.nix
                ./services.nix
                ./jayne/extras.nix
                ./jayne/network.nix
                ./users.nix
            ];
        };

        nixosConfigurations."browncoat" = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
                ./browncoat/hardware-configuration.nix
                ./fonts.nix
                ./header.nix
                ./packages.nix
                ./services.nix
                ./browncoat/extras.nix
                ./browncoat/network.nix
                ./users.nix
            ];
        };

    };

}

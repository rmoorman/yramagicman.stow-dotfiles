{
    description = "A very basic flake";
    inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    outputs = { self, nixpkgs }: {

        nixosConfigurations."tightpants" = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
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
            ];
        };

        nixosConfigurations."jayne" = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
                ./jayne/hardware-configuration.nix
                ./fonts.nix
                ./header.nix
                ./jayne/extras.nix
                ./jayne/network.nix
                ./packages.nix
                ./services.nix
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
                ./systemd/store-scrub.nix
                ./systemd/store-status.nix
                ./systemd/store-balance.nix
            ];
        };

        nixosConfigurations."kaylee" = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
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
            ];
        };

    };

}

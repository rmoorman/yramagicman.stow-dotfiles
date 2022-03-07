{
    description = "A very basic flake";

    outputs = { self, nixpkgs }: {

    # replace 'joes-desktop' with your hostname here.
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
};

}

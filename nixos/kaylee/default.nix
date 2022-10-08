{ config, pkgs, ... }:
{
    imports = [
        ./extras.nix
        ./hardware-configuration.nix
        ./network.nix
    ];
}

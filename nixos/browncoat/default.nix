{ config, pkgs, ... }:
{
  imports = [
    ./extras.nix
    ./filesystems.nix
    ./hardware-configuration.nix
    ./network.nix
    ./nfs.nix
    ./samba.nix
  ];
}

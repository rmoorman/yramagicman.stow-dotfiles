# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
{
    zramSwap.enable = true;

    # Use the systemd-boot EFI boot loader.
    boot.loader.systemd-boot.configurationLimit = 15;
    boot.loader.systemd-boot.enable = true;
    boot.loader.timeout = 1;
    boot.loader.efi.canTouchEfiVariables = true;
    boot.tmp.cleanOnBoot = true;

    boot.kernel.sysctl = {
        "net.ipv6.conf.all.forwarding" = "1";
        "net.ipv4.ip_forward" = "1";
    };

    # Set your time zone.
    time.timeZone = "America/New_York";

    # Select internationalisation properties.
    i18n.defaultLocale = "en_US.UTF-8";
    console = {
        font = "Lat2-Terminus16";
        keyMap = "us";
    };

    nix = {
        package = pkgs.nixUnstable;
        extraOptions = ''
          experimental-features = nix-command flakes
        '';

        gc.automatic = true;
        gc.dates = "18:40" ;
        gc.options = "--delete-older-than 28d";
        settings.auto-optimise-store = true;
        optimise.automatic = true;
        optimise.dates = [ "11:50" "21:40" ];
    };
    system.stateVersion = "23.05";

    networking.nftables.enable = true;
}

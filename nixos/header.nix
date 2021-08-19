# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
{

  system.autoUpgrade.enable = true;
  system.autoUpgrade.dates = "18:00";
  system.autoUpgrade.allowReboot = true;
  nix.gc.automatic = true;
  nix.gc.dates = "18:30";
  nix.gc.options = "--delete-older-than 14d";
  nix.autoOptimiseStore = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.configurationLimit = 15;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  swapDevices = [
      {
          device = "/swapfile";
          size = 4096;
      }
  ];
}

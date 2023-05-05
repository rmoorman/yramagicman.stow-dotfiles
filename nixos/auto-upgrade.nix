{ config, pkgs, ... }:
{

  system.autoUpgrade = {
    enable = true;
    dates = "23:59";
    flake = "${config.users.users.jonathan.home}/Repos/dots";
    flags = [
      "--commit-lock-file"
      "--update-input" "home-manager"
      "--update-input" "nixpkgs"
      "-L"

    ];
    allowReboot = true;
  };




}

{ config, pkgs, ... }:
{

  system.autoUpgrade = {
    enable = true;
    dates = "daily";
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

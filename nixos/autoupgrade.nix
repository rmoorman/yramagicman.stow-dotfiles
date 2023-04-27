{ config, pkgs, ... }:
{
    system.autoUpgrade = {
        enable = true;
        dates = "14:40";
        flake = "${config.users.users.jonathan.home}/Documents/dots";
        flags = [
            "--commit-lock-file"
        ];
        allowReboot = true;
    };
}

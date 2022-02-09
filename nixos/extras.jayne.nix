{ config, pkgs, ... }:
{
    services.xserver.displayManager.gdm.enable = true;
    hardware.acpilight.enable = true;

    services.mysql.ensureDatabases = [
        "ric"
        "testing"
    ];

    services.mysql.ensureUsers = [
        {
            name = "ric";
            ensurePermissions = {
                "ric.*" = "ALL PRIVILEGES";
                "testing.*" = "ALL PRIVILEGES";
            };
        }
    ];

    environment.systemPackages = with pkgs; [
        du-dust
        powertop
        slack
        sqlite
        thunderbird
    ];


    services.flatpak.enable = true;
    xdg.portal.enable = true;
    xdg.portal.extraPortals = [
        pkgs.xdg-desktop-portal-gnome
    ];
}

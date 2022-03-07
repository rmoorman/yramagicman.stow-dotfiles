{ config, pkgs, ... }:
{
    services.xserver.displayManager.gdm.enable = true;
    hardware.acpilight.enable = true;

    httpd.enable = true;
    httpd.adminAddr = "yramagicman@gmail.com";
    httpd.enablePHP = true;
    mysql.package = pkgs.mariadb;
    mysql.enable = true;

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
        # thunderbird
    ];

    swapDevices = [
        {
            device = "/swapfile";
            size = 4096;
        }
    ];

    services.flatpak.enable = true;
    xdg.portal.enable = true;
    xdg.portal.extraPortals = [
        pkgs.xdg-desktop-portal-gnome
    ];
}

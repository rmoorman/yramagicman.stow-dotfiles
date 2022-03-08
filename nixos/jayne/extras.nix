{ config, pkgs, ... }:
{
    hardware.acpilight.enable = true;

    services.httpd.enable = true;
    services.httpd.adminAddr = "yramagicman@gmail.com";
    services.httpd.enablePHP = true;
    services.mysql.package = pkgs.mariadb;
    services.mysql.enable = true;

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

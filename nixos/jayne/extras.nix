{ config, pkgs, ... }:
{
    hardware.acpilight.enable = true;

    services.xserver.displayManager.startx.enable = true;
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
        intel-gpu-tools
        powertop
        slack
        sqlite
        # thunderbird
    ];

    nixpkgs.config.packageOverrides = pkgs: {
        vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
    };

    hardware.opengl = {
        enable = true;
        extraPackages = with pkgs; [
            intel-media-driver # LIBVA_DRIVER_NAME=iHD
            # vaapiIntel       # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
            vaapiVdpau
            libvdpau-va-gl
        ];
    };

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

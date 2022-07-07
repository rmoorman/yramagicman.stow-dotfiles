{ config, pkgs, ... }:
{
    hardware.acpilight.enable = true;

    virtualisation.docker.enable = true;
    users.users.jonathan = {
      extraGroups = [ "docker" ]; # Enable ‘sudo’ for the user.
    };
    services.httpd.enable = true;
    services.httpd.adminAddr = "yramagicman@gmail.com";
    services.httpd.enablePHP = true;
    services.mysql.package = pkgs.mariadb;
    services.mysql.enable = true;
    # services.xserver.desktopManager.mate.enable = true; # keep for zoom calls
    services.httpd.virtualHosts = {
        "api.dev.local" = {

            extraConfig = ''
                ProxyPass /  http://localhost:8000/
                ProxyPassReverse /  http://localhost:8000/
            '';
        };
        "dev.local" = {
            extraConfig = ''
                ProxyPass /  http://localhost:8080/
                ProxyPassReverse /  http://localhost:8080/
            '';
        };

    };

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
        cobang
        dbeaver
        docker-compose
        du-dust
        intel-gpu-tools
        powertop
        slack
        sqlite
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
}

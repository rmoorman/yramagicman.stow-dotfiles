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
        docker-compose
        intel-gpu-tools
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

 # Make some extra kernel modules available to NixOS
  boot.extraModulePackages = with config.boot.kernelPackages;
    [ v4l2loopback.out ];

  # Activate kernel modules (choose from built-ins and extra ones)
  boot.kernelModules = [
    # Virtual Camera
    "v4l2loopback"
    # Virtual Microphone, built-in
    "snd-aloop"
  ];

  # Set initial kernel module settings
  boot.extraModprobeConfig = ''
    # exclusive_caps: Skype, Zoom, Teams etc. will only show device when actually streaming
    # card_label: Name of virtual camera, how it'll show up in Skype, Zoom, Teams
    # https://github.com/umlaeute/v4l2loopback
    options v4l2loopback exclusive_caps=1 card_label="Virtual Camera"
  '';


}

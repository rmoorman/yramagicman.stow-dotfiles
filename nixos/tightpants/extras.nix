{ config, pkgs, ... }:
{
    services.xserver.dpi = 192;
    hardware.acpilight.enable = true;
    boot.kernelPackages = pkgs.linuxPackages_zen;
    environment.systemPackages = with pkgs; [
        dropbox
        system76-firmware
        linuxKernel.packages.linux_zen.system76
        intel-gpu-tools
    ];

    hardware.system76.enableAll = true;
    services.snapper.configs = {
        home = {
            subvolume = "/home";
            extraConfig = ''
                ALLOW_USERS="jonathan"
                TIMELINE_CREATE=yes
                TIMELINE_CLEANUP=yes
            '';
        };
    };

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
}

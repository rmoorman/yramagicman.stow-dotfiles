{ config, pkgs, ... }:
{
    system.autoUpgrade.allowReboot = true;

    # Mount secondary drive
    fileSystems."/home/jonathan/Storage".device = "/dev/disk/by-label/storage";
    boot.kernelPackages = pkgs.linuxPackages_zen;

    environment.systemPackages = with pkgs; [
        dropbox
        btrfs-progs
        intel-gpu-tools
    ];
    services.flatpak.enable = true;
    services.xserver.displayManager.gdm.autoSuspend= false;
    xdg.portal.enable = true;
    xdg.portal.extraPortals = [
        pkgs.xdg-desktop-portal-gnome
    ];

    swapDevices = [
        {
            device = "/swapfile";
            size = 4096;
        }
    ];

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

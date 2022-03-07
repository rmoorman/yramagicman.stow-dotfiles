{ config, pkgs, ... }:
{

    system.autoUpgrade.allowReboot = true;

    services.xserver.displayManager.gdm.enable = true;
    services.xserver.displayManager.gdm.autoSuspend = false;
    # Mount secondary drive
    fileSystems."/home/jonathan/Storage".device = "/dev/disk/by-partuuid/22f96431-a0a9-4524-97fa-3672b86bef5d";
    boot.kernelPackages = pkgs.linuxPackages_zen;

    environment.systemPackages = with pkgs; [
        dropbox
    ];
    services.flatpak.enable = true;
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
}

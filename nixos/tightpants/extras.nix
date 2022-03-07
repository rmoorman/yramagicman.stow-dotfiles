{ config, pkgs, ... }:
{
    services.xserver.displayManager.gdm.enable = true;
    services.xserver.dpi = 192;
    hardware.acpilight.enable = true;
    boot.kernelPackages = pkgs.linuxPackages_zen;
    environment.systemPackages = with pkgs; [
        dropbox
        system76-firmware
        linuxKernel.packages.linux_zen.system76
    ];

    hardware.system76.enableAll = true;
    services.snapper.configs = {
        root = {
            subvolume = "/";
            extraConfig = ''
                ALLOW_USERS="jonathan"
                TIMELINE_CREATE=yes
                TIMELINE_CLEANUP=yes
            '';
        };
        home = {
            subvolume = "/home";
            extraConfig = ''
                ALLOW_USERS="jonathan"
                TIMELINE_CREATE=yes
                TIMELINE_CLEANUP=yes
            '';
        };
    };
}

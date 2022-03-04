{ config, pkgs, ... }:
{
    services.xserver.displayManager.gdm.enable = true;
    services.xserver.dpi = 192;
    hardware.acpilight.enable = true;
    boot.kernelPackages = pkgs.linuxPackages_zen;
    environment.systemPackages = with pkgs; [
      dropbox-cli
      system76-firmware
      linuxKernel.packages.linux_zen.system76
    ];

    hardware.system76.enableAll = true;
}

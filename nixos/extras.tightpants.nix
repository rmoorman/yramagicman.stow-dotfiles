{ config, pkgs, ... }:
{
    services.xserver.displayManager.lightdm.enable = true;
    services.xserver.dpi = 192;
    hardware.acpilight.enable = true;
    boot.kernelPackages = pkgs.linuxPackages_zen;
    environment.systemPackages = with pkgs; [
      dropbox-cli
      system76-firmware
      linuxKernel.packages.linux_zen.system76
    ];

    services.xserver.displayManager.autoLogin.user = "jonathan";
    services.xserver.displayManager.autoLogin.enable = true;
    services.xserver.displayManager.defaultSession ="none+xmonad";
    hardware.system76.enableAll = true;
}

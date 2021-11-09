{ config, pkgs, ... }:
{
    services.xserver.dpi = 192;
    hardware.acpilight.enable = true;
    boot.kernelPackages = pkgs.linuxPackages_zen;

      environment.systemPackages = with pkgs; [
          dropbox-cli
      ];
}

{ config, pkgs, ... }:
{
    fonts.fonts = with pkgs; [
        noto-fonts
        fira-mono
        fira-code
        cantarell-fonts
        corefonts
        winePackages.fonts
    ];

  fonts.fontconfig = {
    enable = true;
    hinting.enable = true;
    hinting.style = "medium";
  };


}

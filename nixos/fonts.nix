{ config, pkgs, ... }:
{
    fonts.fonts = with pkgs; [
        fira-mono
        cantarell-fonts
        corefonts
        winePackages.fonts
    ];
}

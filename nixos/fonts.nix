{ config, pkgs, ... }:
{
    fonts.fonts = with pkgs; [
        fira-mono
        fira-code
        cantarell-fonts
        corefonts
        winePackages.fonts
    ];
}

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
}

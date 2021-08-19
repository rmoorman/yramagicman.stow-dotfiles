{ config, pkgs, ... }:
{
    fonts.fonts = with pkgs; [
        fira-mono
        cantarell-fonts
    ];
}

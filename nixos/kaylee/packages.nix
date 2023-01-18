{ config, pkgs, lib, ... }:
{
  home.packages = with pkgs; [
    openrgb
    gphoto2
    asunder
    wezterm
    handbrake
  ];
}

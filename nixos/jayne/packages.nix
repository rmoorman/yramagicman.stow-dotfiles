{ config, pkgs, lib, ... }:
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "jonathan";
  home.homeDirectory = "/home/jonathan";

  home.packages = with pkgs; [
        cobang
        dbeaver
        du-dust
        gphoto2
        ngrok
        powertop
        slack
        sqlite
  ];
}

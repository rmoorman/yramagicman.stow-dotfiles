{ config, pkgs, lib, ... }:
{

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

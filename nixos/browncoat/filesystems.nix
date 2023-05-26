{ config, pkgs, ... }:
{

  # Mount secondary drive
  fileSystems."/home/jonathan/Storage".device = "/dev/disk/by-label/storage";
  fileSystems."/home/jonathan/Storage".options = ["compress=zstd"];

  fileSystems."/srv/music" = {
    device = "/home/jonathan/Music";
    options = ["bind"];
  };

  fileSystems."/srv/video" = {
    device = "/home/jonathan/Videos";
    options = ["bind"];
  };

  fileSystems."/srv/storage" = {
    device = "/home/jonathan/Storage";
    options = ["bind"];
  };

  fileSystems."/srv/jonathan" = {
    device = "/srv/storage/Jonathan";
    options = ["bind"];
  };
  fileSystems."/srv/home" = {
    device = "/home/jonathan/";
    options = ["bind"];
  };
}

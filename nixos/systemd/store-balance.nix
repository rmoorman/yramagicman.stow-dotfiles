{ config, pkgs, ... }:
{
  systemd.services = {
    store-balance = {
      after = [ "network-online.target" ];
      description = "Run btrfs balance yearly";
      enable=true;
      script = "/run/current-system/sw/bin/btrfs balance start -dusage=50 -dlimit=2 -musage=50 -mlimit=4 /srv/storage/";
      serviceConfig.Type = "simple";
      startAt = "*-01-15 00:00:00";
      wants = [ "network-online.target" ];
    };
  };
  systemd.timers.store-balance = {
    timerConfig = {
      Persistent = true;
    };
  };
}

{ config, pkgs, ... }:
{
  systemd.services = {
    store-balance = {
      after = [ "network-online.target" ];
      description = "Run btrfs balance monthly";
      enable=true;
      script = "/run/current-system/sw/bin/btrfs balance start --full-balance /srv/storage/";
      serviceConfig.Type = "oneshot";
      startAt = "*-*-15 00:00:00";
      wants = [ "network-online.target" ];
    };
  };
  systemd.timers.store-balance = {
    timerConfig = {
      Persistent = true;
    };
  };
}

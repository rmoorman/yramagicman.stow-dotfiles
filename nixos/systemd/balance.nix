{ config, pkgs, ... }:
{
  systemd.services = {
    btrfs-balance = {
      after = [ "network-online.target" ];
      description = "Run btrfs balance monthly";
      enable=true;
      script = "/run/current-system/sw/bin/btrfs balance start --full-balance /";
      serviceConfig.Type = "oneshot";
      startAt = "*-*-15 00:00:00";
      wants = [ "network-online.target" ];
    };
  };
  systemd.timers.btrfs-balance = {
    timerConfig = {
      Persistent = true;
    };
  };

}

{ config, pkgs, ... }:
{
  systemd.services = {
    btrfs-balance = {
      after = [ "network-online.target" ];
      description = "Run btrfs balance yearly";
      enable=true;
      script = "/run/current-system/sw/bin/btrfs balance start -dusage=50 -dlimit=2 -musage=50 -mlimit=4 /";
      serviceConfig.Type = "simple";
      startAt = "*-01-15 23:00:00";
      wants = [ "network-online.target" ];
    };
  };
  systemd.timers.btrfs-balance = {
    timerConfig = {
      Persistent = true;
    };
  };

}

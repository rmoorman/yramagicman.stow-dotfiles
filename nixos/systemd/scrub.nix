{ config, pkgs, ... }:
{
  systemd.services = {
    btrfs-scrub = {
      after = [ "network-online.target" ];
      description = "Run btrfs scrub monthly";
      enable=true;
      script = "/run/current-system/sw/bin/btrfs scrub start /";
      serviceConfig.Type = "oneshot";
      startAt = "monthly";
      wants = [ "network-online.target" ];
    };
  };

  systemd.timers.btrfs-scrub = {
    timerConfig = {
      Persistent = true;
    };
  };
}

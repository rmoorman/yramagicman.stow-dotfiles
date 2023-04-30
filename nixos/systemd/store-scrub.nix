{ config, pkgs, ... }:
{

  systemd.services = {
    store-scrub = {
      after = [ "network-online.target" ];
      description = "Run btrfs scrub monthly";
      enable=true;
      script = "/run/current-system/sw/bin/btrfs scrub start /srv/storage/";
      serviceConfig.Type = "oneshot";
      startAt = "monthly";
      wants = [ "network-online.target" ];
    };
  };

  systemd.timers.store-scrub = {
    timerConfig = {
      Persistent = true;
    };
  };

}

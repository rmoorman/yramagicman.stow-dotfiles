
{ config, pkgs, ... }:
{
  systemd.timers = {
    "store-status" = {
      wantedBy = [ "timers.target" ];
      after = [ "time-set.target" "time-sync.target" ];
      partOf=[ "store-status.service" ];
      enable = true;
      timerConfig = {
        Unit = "store-status.service";
        OnCalendar = "monthly";
        Persistent=true;
      };
    };
  };

  systemd.services = {
    "store-status" = {
      description = "Check status of btrfs scrub";
      serviceConfig.Type = "oneshot";
      partOf=[ "store-status.service" ];
      script = "/run/current-system/sw/bin/btrfs scrub status /dev/disk/by-label/storage > /home/disk-check";
      wantedBy = [ "store-status.timer" ];
    };
  };
}

{ config, pkgs, ... }:
{
    systemd.timers = {
        "status" = {
            wantedBy = [ "timers.target" ];
            after = [ "time-set.target" "time-sync.target" ];
            partOf=[ "disk-check.service" ];
            enable = true;
            timerConfig = {
                Unit = "disk-check.service";
                OnCalendar = "monthly";
                Persistent=true;
            };
        };
    };

    systemd.services = {
        "disk-check" = {
            description = "Check status of btrfs scrub";
            partOf=[ "disk-check.service" ];
            script = "/run/current-system/sw/bin/btrfs scrub status /dev/disk/by-label/nixos > /home/disk-check";
            wantedBy = [ "status.timer" ];
        };
    };
}

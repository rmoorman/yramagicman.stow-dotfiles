{ config, pkgs, ... }:
{
    systemd.timers = {
        "scrub" = {
            wantedBy = [ "timers.target" ];
            enable = true;
            timerConfig = {
                OnBootSec= "1m";
                Unit = "btrfs-scrub.service";
                OnCalendar = "monthly";
            };
        };

        "status" = {
            wantedBy = [ "timers.target" ];
            enable = true;
            timerConfig = {
                OnBootSec= "1m";
                Unit = "disk-check.service";
                OnCalendar = "*-*-02 00:00:00";
            };
        };


    };

    systemd.services = {
        "btrfs-scrub.service" = {
            description = "Run btrfs scrub monthly";
            script = "/run/current-system/sw/bin/btrfs scrub start /dev/disk/by-label/storage";
            wantedBy = [ "scrub.timer" ];
        };

        "disk-check" = {
            description = "Check status of btrfs scrub";
            script = "/run/current-system/sw/bin/btrfs scrub status /dev/disk/by-label/storage > /home/jonathan/disk-check";
            wantedBy = [ "status.timer" ];
        };
    };
}

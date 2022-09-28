{ config, pkgs, ... }:
{
    systemd.timers = {

        "scrub" = {
            wantedBy = [ "timers.target" ];
            enable = true;
            after = [ "time-set.target" "time-sync.target" ];
            partOf=[ "btrfs-scrub.service" ];
            timerConfig = {
                Unit = "btrfs-scrub.service";
                OnCalendar = "monthly";
                Persistent=true;
            };
        };
    };

    systemd.services = {
        "btrfs-scrub" = {
            partOf=[ "btrfs-scrub.service" ];
            description = "Run btrfs scrub monthly";
            script = "/run/current-system/sw/bin/btrfs scrub start /";
            wantedBy = [ "scrub.timer" ];
        };
    };

}

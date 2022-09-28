{ config, pkgs, ... }:
{
    systemd.timers = {

        "scrub" = {
            wantedBy = [ "timers.target" ];
            enable = true;
            after = [ "time-set.target" "time-sync.target" ];
            partOf=[ "btrfs-scrub-store.service" ];
            timerConfig = {
                Unit = "btrfs-scrub-store.service";
                OnCalendar = "monthly";
                Persistent=true;
            };
        };
    };

    systemd.services = {
        "btrfs-scrub-store" = {
            partOf=[ "btrfs-scrub-store.service" ];
            description = "Run btrfs scrub monthly";
            script = "/run/current-system/sw/bin/btrfs scrub start /dev/disk/by-label/storage";
            wantedBy = [ "scrub.timer" ];
        };
    };

}

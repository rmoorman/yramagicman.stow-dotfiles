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
            serviceConfig.Type = "oneshot";
            partOf=[ "btrfs-scrub-store.service" ];
            description = "Run btrfs scrub monthly";
            script = "/run/current-system/sw/bin/btrfs scrub start /srv/storage/";
            wantedBy = [ "scrub.timer" ];
        };
    };

}

{ config, pkgs, ... }:
{
    systemd.timers = {

        "store-balance" = {
            wantedBy = [ "timers.target" ];
            enable = true;
            after = [ "time-set.target" "time-sync.target" ];
            partOf=[ "btrfs-store-balance.service" ];
            timerConfig = {
                Unit = "btrfs-store-balance.service";
                OnCalendar = "*-*-15 00:00:00";
                Persistent=true;
            };
        };
    };

    systemd.services = {
        "btrfs-store-balance" = {
            partOf=[ "btrfs-store-balance.service" ];
            serviceConfig.Type = "oneshot";
            description = "Run btrfs balance monthly";
            script = "/run/current-system/sw/bin/btrfs balance start --full-balance /srv/storage/";
            wantedBy = [ "store-balance.timer" ];
        };
    };

}

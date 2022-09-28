{ config, pkgs, ... }:
{
    systemd.timers = {

        "balance" = {
            wantedBy = [ "timers.target" ];
            enable = true;
            after = [ "time-set.target" "time-sync.target" ];
            partOf=[ "btrfs-balance.service" ];
            timerConfig = {
                Unit = "btrfs-balance.service";
                OnCalendar = "*-*-15 00:00:00";
                Persistent=true;
            };
        };
    };

    systemd.services = {
        "btrfs-balance" = {
            partOf=[ "btrfs-balance.service" ];
            description = "Run btrfs balance monthly";
            script = "/run/current-system/sw/bin/btrfs balance start --full-balance /";
            wantedBy = [ "balance.timer" ];
        };
    };

}

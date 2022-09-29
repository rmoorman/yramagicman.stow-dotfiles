{ config, pkgs, ... }:
{
    systemd.timers = {
        btrfs-balance = {
            enable=false;
            wantedBy = [ "timers.target" ];
            timerConfig = {
                OnCalendar = "*-*-15 00:00:00";
                Persistent=true;
            };
        };
    };

    systemd.services = {
        btrfs-balance = {
            enable=false;
            serviceConfig.Type = "oneshot";
            description = "Run btrfs balance monthly";
            script = "/run/current-system/sw/bin/btrfs balance start --full-balance /";
            after = [ "time-set.target" "time-sync.target" ];
        };
    };

}

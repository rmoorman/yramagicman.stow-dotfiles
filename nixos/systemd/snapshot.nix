{ config, pkgs, ... }:
{
    systemd.timers = {
        "snap" = {
            wantedBy = [ "timers.target" ];
            enable = true;
            timerConfig = {
                OnBootSec= "1m";
                Unit = "home-snapshot.service";
                OnCalendar = "hourly";
            };
        };
    };

    systemd.services = {
        "home-snapshot" = {
            description = "take snapshot of home directory";
            path=[
                "/run/current-system/sw/"
                "/run/wrappers/"
            ];
            script = "/opt/home-snapshot";
            wantedBy = [ "snap.timer" ];
        };

    };
}
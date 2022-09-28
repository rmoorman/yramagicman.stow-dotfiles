{ config, pkgs, ... }:
{
    systemd.timers = {

        "backup" = {
            wantedBy = [ "timers.target" "network.target" ];
            enable = true;
            timerConfig = {
                Unit = "backup.service";
                OnBootSec= "5m";
            };
        };
    };


    systemd.services = {
        "backup" = {
            description = "Backup home directory to local server";
            script = "/opt/home-backup";
            wantedBy = [ "backup.timer" ];
        };
    };
}

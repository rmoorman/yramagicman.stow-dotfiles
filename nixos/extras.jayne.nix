{ config, pkgs, ... }:
{
    services.xserver.displayManager.gdm.enable = true;
    hardware.acpilight.enable = true;

    services.mysql.ensureDatabases = [
        "ric"
        "testing"
    ];

    services.mysql.ensureUsers = [
        {
            name = "ric";
            ensurePermissions = {
                "ric.*" = "ALL PRIVILEGES";
                "testing.*" = "ALL PRIVILEGES";
            };
        }
    ];

    virtualisation.virtualbox.host.enable = true;
}

{ config, pkgs, lib, ... }:
{
  system.activationScripts.makeRediaryDir = lib.stringAfter [ "var" ] '' mkdir -p /srv/rediary/ '';
  virtualisation.oci-containers = {
    containers = {
      "rediary" = {
        autoStart = true;
        image = "aceberg/rediary";
        ports = [
          "8847:8847"
        ];
        volumes = [
          "/srv/rediary/:/data/rediary"
        ];

        environment = {
          TZ = "America/New_York" ;
          # DB: "/data/rediary/sqlite.db" ;      # optional, default: /data/rediary/sqlite.db
          # HOST: "0.0.0.0"                     # optional, default: 0.0.0.0
          # PORT: "8847"                        # optional, default: 8847
          # THEME: "minty"                      # optional, default: minty
        };
      };
    };
  };

  networking.firewall.allowedTCPPorts = [
    8847
  ];

  networking.firewall.allowedUDPPorts = [
    8847
  ];

}

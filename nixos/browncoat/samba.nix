{ config, pkgs, ... }:
{

  services.samba.openFirewall = true;
  services.samba-wsdd.enable = true; # make shares visible for windows 10 clients
  services.samba = {
    enable = true;
    securityType = "user";
    extraConfig = ''
            workgroup = WORKGROUP
            server string = browncoat
            netbios name = browncoat
            security = user
            guest account = nobody
            map to guest = bad user
            smbd profiling support = on
        '';
    shares = {
      public = {
        path = "/srv/storage/Public";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "jonathan";
        "force group" = "users";
      };

      root = {
        path = "/srv/storage/";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "no";
        "valid users" = "jonathan";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "jonathan";
        "force group" = "users";
      };

      jonathan = {
        path = "/srv/jonathan/";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "no";
        "valid users" = "jonathan";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "jonathan";
        "force group" = "users";
      };

      music = {
        path = "/srv/music/";
        browseable = "yes";
        "read only" = "yes";
        "guest ok" = "yes";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "jonathan";
        "force group" = "users";
      };

      video = {
        path = "/srv/video/";
        browseable = "yes";
        "read only" = "yes";
        "guest ok" = "yes";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "jonathan";
        "force group" = "users";
      };

      Emily = {
        path = "/srv/storage/Emily";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "no";
        "create mask" = "0644";
        "directory mask" = "0755";
      };
    };
  };

  networking.firewall.allowedTCPPorts = [
    5357 # wsdd/samba
  ];

  networking.firewall.allowedUDPPorts = [
    3702 # wsdd/samba
  ];
}

{ config, pkgs, ... }:
{
  networking.hostName = "browncoat"; # Define your hostname.
  networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.defaultGateway.address = "192.168.1.1";
  networking.nameservers = [
      "192.168.1.1"
      "1.1.1.1"
      "1.0.0.1"

  ];
  networking.interfaces.enp4s0.ipv4.addresses = [
    {
      address = "192.168.1.224";
      prefixLength = 24;
    }
  ];
  # networking.interfaces.enp0s31f6.ipv4.addresses = [
  #     {
  #         address = "192.168.1.203";
  #         prefixLength = 24;
  #     }
  # ];

  fileSystems."/srv/music" = {
    device = "/home/jonathan/Music";
    options = ["bind"];
  };

  fileSystems."/srv/video" = {
    device = "/home/jonathan/Videos";
    options = ["bind"];
  };

  fileSystems."/srv/storage" = {
    device = "/home/jonathan/Storage";
    options = ["bind"];
  };

  fileSystems."/srv/jonathan" = {
    device = "/srv/storage/Jonathan";
    options = ["bind"];
  };
  fileSystems."/srv/home" = {
    device = "/home/jonathan/";
    options = ["bind"];
  };

   services.nfs.server.enable=true;
   services.nfs.server.exports = ''
         /srv 192.168.1.0/24(rw,sync,crossmnt,fsid=0)
         /srv/music 192.168.1.0/24(rw,sync)
         /srv/video 192.168.1.0/24(rw,sync)
         /srv/storage 192.168.1.0/24(rw,sync)
         /srv/home 192.168.1.0/24(rw,sync)
     '';
  services.samba-wsdd.enable = true; # make shares visible for windows 10 clients
  networking.firewall.allowedTCPPorts = [
    5357 # wsdd/samba
    2049 # nfs
    8443
    19999 #netdata
    80
    443
    5001
    631
  ];
  networking.firewall.allowedUDPPorts = [
    3702 # wsdd/samba
    5001
    631
  ];

  services.samba = {
    enable = true;
    securityType = "user";
    extraConfig = ''
            workgroup = WORKGROUP
            server string = smbnix
            netbios name = smbnix
            security = user
            guest account = nobody
            map to guest = bad user
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

  networking.firewall.allowPing = true;
  services.samba.openFirewall = true;
  services.tailscale.port=48612;

  services.jellyfin.enable = true;
  services.jellyfin.openFirewall = true;
}

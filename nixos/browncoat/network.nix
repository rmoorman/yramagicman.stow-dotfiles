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

  networking.firewall.allowedTCPPorts = [
    8443
    19999 #netdata
    80
    443
    5001
    631
  ];
  networking.firewall.allowedUDPPorts = [
    5001
    631
  ];


  networking.firewall.allowPing = true;
  services.tailscale.port=48612;

  services.jellyfin.enable = true;
  services.jellyfin.openFirewall = true;
}

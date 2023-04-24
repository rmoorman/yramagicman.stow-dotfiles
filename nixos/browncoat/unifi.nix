{ config, pkgs, ... }:
{
  virtualisation.oci-containers = {
    containers = {
      "unifi" = {
        image = "linuxserver/unifi-controller";
        ports = [
          "8443:8443"
          "3478:3478/udp"
          "10001:10001/udp"
          "8080:8080"
          "1900:1900/udp"
          "8843:8843"
          "8880:8880"
          "6789:6789"
          "5514:5514/udp"
        ];
      };
    };
  };

  networking.firewall.allowedTCPPorts = [
    19999
    2049 # nfs
    443
    5357 # wsdd
    6789
    80
    8080
    8443
    8843
    8880
  ];

  networking.firewall.allowedUDPPorts = [
    3702 # wsdd
    3478
    10001
    1900
    5514
  ];

}

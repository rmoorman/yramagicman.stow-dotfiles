{ config, pkgs, ... }:
{
  virtualisation.oci-containers = {
    containers = {
      "pihole" = {
        autoStart = true;
        image = "pihole/pihole:latest";
        ports = [
          "53:53/udp"
          "53:53/tcp"
          "8080:80/tcp"
        ];
        volumes = [
          "/srv/pihole/etc/pihole:/etc/pihole"
          "/srv/pihole/etc/dnsmasq.d:/etc/dnsmasq.d"
        ];

        # extraOptions = [ "--hostname pihole" ];
        environment = {
          WEBPASSWORD =  "fcXC2zkU5y8zvRuFugb9k9zOoIbwkkCEXOsxdvCCwNFd3IomyLBFIVfiLz4j";
        };
      };
    };
  };

  networking.firewall.allowedTCPPorts = [
    53
    8080
  ];

  networking.firewall.allowedUDPPorts = [
    53
    8080
  ];

}

{ config, pkgs, ... }:
{
  services.nfs.server.enable=true;
  services.rpcbind.enable = true;
  services.nfs.server.exports = ''
         /srv 192.168.1.0/24(rw,sync,crossmnt,fsid=0)
         /srv/music 192.168.1.0/24(rw,sync)
         /srv/video 192.168.1.0/24(rw,sync)
         /srv/storage 192.168.1.0/24(rw,sync)
         /srv/home 192.168.1.0/24(rw,sync)
     '';

  networking.firewall.allowedTCPPorts = [
    2049 # nfs
  ];

  networking.firewall.allowedUDPPorts = [
    2049 # nfs
  ];

}

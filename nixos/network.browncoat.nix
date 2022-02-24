{ config, pkgs, ... }:
{
    networking.hostName = "browncoat"; # Define your hostname.
    networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    networking.useDHCP = false;
    networking.interfaces.enp0s31f6.useDHCP = true;
    # networking.interfaces.enp5s0.ipv4.addresses = [
    #     {
    #         address = "127.0.0.10";
    #         prefixLength = 24;
    #     }
    # ];
    # networking.interfaces.wlp6s0.ipv4.addresses = [
    #     {
    #         address = "127.0.0.11";
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
    services.nfs.server.enable=true;
    services.nfs.server.exports = ''
        /srv 192.168.1.0/24(rw,sync,crossmnt,fsid=0)
        /srv/music 192.168.1.0/24(rw,sync)
        /srv/video 192.168.1.0/24(rw,sync)
    '';
    networking.firewall.allowedTCPPorts = [2049];
}

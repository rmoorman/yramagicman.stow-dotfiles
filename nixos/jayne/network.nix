{ config, pkgs, ... }:
{
    networking.hostName = "jayne"; # Define your hostname.
    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
    networking.wireless.iwd.enable = true;  # Enables wireless support via iwd.
    # networking.networkmanager.wifi.backend = "iwd";

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    networking.useDHCP = false;
    # networking.interfaces.enp1s0.useDHCP = true;
    # networking.interfaces.wlp2s0.useDHCP = true;
    # networking.interfaces.wlan0.useDHCP = true;

    # Configure network proxy if necessary
    # networking.proxy.default = "http://user:password@proxy:port/";
    # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

    networking.hosts = {
        "192.168.1.196" = ["browncoat"];
        "127.0.0.1" = ["api.dev.local" "dev.local" ];
    };

    # fileSystems."/home/jonathan/Music" = {
    #     device ="browncoat:/music";
    #     fsType="nfs";
    #     options = ["x-systemd.automount" "noauto"];
    # };

    systemd.network = {
        enable = true;
        networks = {
            "25-wireless" = {
                dns = ["1.1.1.1" "1.0.0.1"];
                matchConfig = { Name = "wlan0"; };
                DHCP= "yes";
            };

            "26-wireless" = {
                dns = ["1.1.1.1" "1.0.0.1"];
                matchConfig = { Name = "enp1s0"; };
                DHCP= "yes";
            };


        };
    };



}

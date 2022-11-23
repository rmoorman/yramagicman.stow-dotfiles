{ config, pkgs, ... }:
{
    networking.hostName = "jayne"; # Define your hostname.
    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
    networking.wireless.iwd.enable = true;  # Enables wireless support via iwd.
    # networking.networkmanager.wifi.backend = "iwd";
    networking.useDHCP = true;
    # Configure network proxy if necessary
    # networking.proxy.default = "http://user:password@proxy:port/";
    # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

    networking.hosts = {
        "192.168.1.203" = ["browncoat"];
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
                matchConfig = { Name = "wlan0"; };
                DHCP= "yes";
            };

            "26-wired" = {
                matchConfig = { Name = "enp1s0"; };
                DHCP= "yes";
            };

        };
    };
    services.tailscale.port=48613;
}

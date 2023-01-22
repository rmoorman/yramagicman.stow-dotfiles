{
    networking.hostName = "tightpants"; # Define your hostname.
    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
    networking.wireless.iwd.enable = true;  # Enables wireless support via iwd.
    # networking.networkmanager.wifi.backend = "iwd";
    # networking.networkmanager.enable = false;

    # networking.useDHCP = true;

    # Configure network proxy if necessary
    # networking.proxy.default = "http://user:password@proxy:port/";
    # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

    # networking.hosts = {
    #     "192.168.1.203" = ["browncoat"];
    # };

    # fileSystems."/home/jonathan/Music" = {
    #     device ="browncoat:/music";
    #     fsType="nfs";
    #     options = ["x-systemd.automount" "noauto"];
    # };

    systemd.network = {
        enable = true;
        networks = {
            "25-wireless" = {
                matchConfig = { Match = "wlan*"; };
                DHCP= "yes";
            };
        };
    };

    services.tailscale.port=48614;
}

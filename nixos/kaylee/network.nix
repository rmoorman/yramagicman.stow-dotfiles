{
    networking.hostName = "kaylee"; # Define your hostname.
    networking.wireless.iwd.enable = true;  # Enables wireless support via iwd.
    networking.useDHCP = true;

    # Configure network proxy if necessary
    # networking.proxy.default = "http://user:password@proxy:port/";
    # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

    systemd.network = {
        enable = true;
        networks = {
            "90-all" = {
                matchConfig = { Name = "*"; };
                DHCP= "yes";
            };
        };
    };

    services.tailscale.port=48615;
}

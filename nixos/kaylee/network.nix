{
    networking.hostName = "kaylee"; # Define your hostname.
    networking.wireless.iwd.enable = true;  # Enables wireless support via iwd.
    networking.useDHCP = false;

    # Configure network proxy if necessary
    # networking.proxy.default = "http://user:password@proxy:port/";
    # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  services.udev.extraRules = ''
    KERNEL=="wlan*", ATTR{address}=="34:c9:3d:0f:e7:63", NAME="wlan0"
  '';

    systemd.network = {
        enable = true;
        networks = {
            "90-wireless" = {
                matchConfig = { Name = "wlan*"; };
                DHCP= "yes";
            };

            # "80-wired" = {
            #     matchConfig = { Name = "enp*s*"; };
            #     DHCP= "yes";
            # };
        };
    };

    services.tailscale.port=48615;
}

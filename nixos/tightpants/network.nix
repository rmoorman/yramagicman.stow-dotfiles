{
  networking.hostName = "tightpants"; # Define your hostname.
  networking.wireless.iwd.enable = true;  # Enables wireless support via iwd.

  networking.useDHCP = false;

  services.udev.extraRules = ''
    KERNEL=="wlan*", ATTR{address}=="80:fa:5b:5c:be:bc", NAME="wlan0"
  '';

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  systemd.network = {
    enable = true;
    networks = {
      "90-wireless" = {
        matchConfig = { Name = "wlan*"; };
        DHCP= "yes";
      };
    };
  };

  services.tailscale.port=48614;
}

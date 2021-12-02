{
    networking.hostName = "browncoat"; # Define your hostname.
    networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    networking.useDHCP = false;
    networking.interfaces.enp0s31f6.useDHCP = true;
    networking.interfaces.enp5s0.ipAddress = "127.0.0.10";
    networking.interfaces.wlp6s0.useDHCP = "127.0.0.11";
}

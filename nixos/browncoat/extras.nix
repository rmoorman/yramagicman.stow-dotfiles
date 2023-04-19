{ config, pkgs, ... }:
{

  # Mount secondary drive
  fileSystems."/home/jonathan/Storage".device = "/dev/disk/by-label/storage";
  fileSystems."/home/jonathan/Storage".options = ["compress=zstd"];

  environment.systemPackages = with pkgs; [
    btrfs-progs
    intel-gpu-tools
    podman-compose
    microcodeIntel
  ];
  # services.flatpak.enable = true;
  # xdg.portal.enable = true;
  # xdg.portal.extraPortals = [
  #     pkgs.xdg-desktop-portal-gnome
  # ];

  virtualisation.podman = {
    enable = true;
    # autoPrune.enable = true;
    dockerCompat = true;

    # Required for containers under podman-compose to be able to talk to each other.
    defaultNetwork.dnsname.enable = true;
  };

  swapDevices = [
    {
      device = "/swapfile";
      size = 4096;
    }
  ];

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      # vaapiIntel       # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  security.sudo.extraRules = [
    {
      users = [ "jonathan" "tomg" ];
      commands = [
        { command = "/opt/receive-backup"; options = [ "NOPASSWD" ]; }
        { command = "/opt/dad-backup"; options = [ "NOPASSWD" ]; }
      ];
    }
  ];

  services.apcupsd.enable = true;

  users.users.tomg = {
    isNormalUser = true;
    home  = "/home/tomg";
    createHome  = true;
    extraGroups = [ "audio" "video" "kvm" "wheel" ];
    shell = pkgs.zsh;
    initialPassword = "v/d7xBMgrZnLO[H[W`6z:3Ru@}";
  };

  services.nextcloud = {
    enable = true;
    hostName = "browncoat.local";
    home = "/srv/storage/nextcloud/home/";
    datadir = "/srv/storage/nextcloud/data/";
    config.adminpassFile = "${pkgs.writeText "adminpass" "test123"}";
    package= pkgs.nextcloud25;
    enableBrokenCiphersForSSE = false;

    config.extraTrustedDomains = [
      "100.109.94.104"
      "100.124.185.19"
      "100.125.24.87"
      "100.75.20.127"
      "100.87.66.73"
      "100.94.223.34"
    ];
  };
}

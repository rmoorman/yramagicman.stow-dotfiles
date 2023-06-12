{ config, pkgs, ... }:
let sharedDirectories = [
      "Documents"
      "Calibre Library"
      "Pictures"
      "Videos"
      "Music"
      "Sites"
    ];
in
{

  users.users.jonathan.extraGroups = ["jellyfin"];
  system.autoUpgrade = {
    enable = true;
    dates = "weekly";
    flake = "${config.users.users.jonathan.home}/Repos/dots/nixos/browncoat";
    flags = [
      "--commit-lock-file"
      "--update-input" "home-manager"
      "--update-input" "nixpkgs"
      "-L"
    ];
    allowReboot = true;
  };


  services.netdata.enable = true;

  environment.systemPackages = with pkgs; [
    btrfs-progs
    intel-gpu-tools
    podman-compose
    microcodeIntel
    emacs-nox
    pass
    iftop
  ];

  virtualisation.podman = {
    enable = true;
    autoPrune.enable = true;
    dockerCompat = true;

    # Required for containers under podman-compose to be able to talk to each other.
    # defaultNetwork.settings.dns_enabled = true;
  };

  swapDevices = [
    {
      device = "/swapfile";
      size = 4096;
    }
  ];
  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };

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
    {
      users = ["netdata"];
      commands = [
        { command = "/run/current-system/sw/bin/smbstatus"; options = [ "NOPASSWD" ]; }
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

  users.users.emily = {
    isNormalUser = true;
    createHome  = false;
    shell = pkgs.shadow;
    group="users";
  };

  services.nextcloud = {
    enable = true;
    hostName = "browncoat.local";
    home = "/srv/storage/nextcloud/home/";
    datadir = "/srv/storage/nextcloud/data/";
    config.adminpassFile = "${pkgs.writeText "adminpass" "test123"}";
    package= pkgs.nextcloud26;
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

  services.syncthing = {
    user = "jonathan";
    group="users";
    enable = true;
    dataDir = "/home/syncthing";
    guiAddress = "0.0.0.0:8384";

    extraOptions= {
      gui = {
        user = "jonathan";
        password = "syncmystuff";
      };
    };
    devices = {
      "kaylee" = { id = "RQZIUDO-R6463VZ-M5SSAUF-M4IYNFZ-HWVSZBL-JCBWNK4-X2WIWVU-KZNFOAR"; };
    };
    folders = builtins.listToAttrs ( map (f:
      {
        name="${f}";
        value = (builtins.listToAttrs [
          {name="path"; value="${config.users.users.jonathan.home}/${f}";}
          {name="devices"; value=["kaylee"];}
        ]);
      }) sharedDirectories ) ;
  };

  networking.firewall.allowedTCPPorts = [
    8384
  ];

  services.printing.browsing = true;
  services.printing.listenAddresses = [ "*:631" ]; # Not 100% sure this is needed and you might want to restrict to the local network
  services.printing.allowFrom = [ "all" ]; # this gives access to anyone on the interface you might want to limit it see the official documentation

}

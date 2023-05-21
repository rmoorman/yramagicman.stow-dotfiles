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
    ];
    allowReboot = true;
  };

  # Mount secondary drive
  fileSystems."/home/jonathan/Storage".device = "/dev/disk/by-label/storage";
  fileSystems."/home/jonathan/Storage".options = ["compress=zstd"];

  services.netdata.enable = true;

  environment.systemPackages = with pkgs; [
    btrfs-progs
    intel-gpu-tools
    podman-compose
    microcodeIntel
    emacs-nox
  ];

  virtualisation.podman = {
    enable = true;
    # autoPrune.enable = true;
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
      intel-compute-runtime
      intel-media-driver
      libvdpau-va-gl
      vaapiIntel
      vaapiVdpau
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

    settings = {
      gui = {
        user = "jonathan";
        password = "syncmystuff";
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
  };

  networking.firewall.allowedTCPPorts = [
    8384
  ];

}

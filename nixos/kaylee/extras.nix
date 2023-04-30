{ config, pkgs, ... }:
{


  boot.kernelPackages = pkgs.linuxPackages_zen;
  # boot.extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback.out ];

  # boot.kernelModules = [
  #   # Virtual Camera
  #   "v4l2loopback"
  #   # Virtual Microphone, built-in
  #   "snd-aloop"
  # ];

  # # Set initial kernel module settings
  # boot.extraModprobeConfig = ''
  #   # exclusive_caps: Skype, Zoom, Teams etc. will only show device when actually streaming
  #   # card_label: Name of virtual camera, how it'll show up in Skype, Zoom, Teams
  #   # https://github.com/umlaeute/v4l2loopback
  #   options v4l2loopback exclusive_caps=1 card_label="Virtual Camera"
  # '';

  fileSystems."/".options = ["compress=zstd"];

  environment.systemPackages = with pkgs; [
    amdvlk
    microcodeAmd
    xorg.xf86videoamdgpu
    virt-manager
  ];

  # virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;
  services.flatpak.enable = true;
  services.syncthing = {
    user = "jonathan";
    group="users";
    enable = false;
    dataDir = "/home/syncthing";
    guiAddress = "0.0.0.0:8384";

    extraOptions= {
      gui = {
        user = "jonathan";
        password = "syncmystuff";
      };
    };

    devices = {
      "browncoat" = { id = "H2CBPQ3-VYQ7GUS-TFJEPMO-MBSUUI2-ACPLSCP-PLDH5IZ-P3XJN4B-HLPXMAE"; };
    };
    folders = {
      "home" = {
        path = "/home/jonathan/";    # Which folder to add to Syncthing
        devices = [ "browncoat" ];      # Which devices to share the folder with
      };
    };
  };

  networking.firewall.allowedTCPPorts = [
    8384
  ];

  #nixpkgs.config.packageOverrides = pkgs: {
  #    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  #};

  #hardware.opengl = {
  #    enable = true;
  #    extraPackages = with pkgs; [
  #        intel-media-driver # LIBVA_DRIVER_NAME=iHD
  #        # vaapiIntel       # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
  #        vaapiVdpau
  #        libvdpau-va-gl
  #    ];
  #};
}

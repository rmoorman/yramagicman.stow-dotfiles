{ config, pkgs, ... }:
let sharedDirectories = [
      "Documents"
      "Calibre Library"
      "Pictures"
      "Videos"
      "Music"
    ];
in
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


  fileSystems."/home/jonathan/Storage" = {
      device = "//100.94.223.34/public";
      fsType = "cifs";
      options = let
        # this line prevents hanging on network split
        automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s,uid=1000,gid=100";

      in ["${automount_opts}"];
  };

  programs.dconf.enable = true;
  services.flatpak.enable = true;
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
      "browncoat" = { id = "H2CBPQ3-VYQ7GUS-TFJEPMO-MBSUUI2-ACPLSCP-PLDH5IZ-P3XJN4B-HLPXMAE"; };
    };

    folders = builtins.listToAttrs ( map (f:
      {
        name="${f}";
        value = (builtins.listToAttrs [
          {name="path"; value="${config.users.users.jonathan.home}/${f}";}
          {name="devices"; value=["browncoat"];}
        ]);
      }) sharedDirectories ) ;
  };

  networking.firewall.allowedTCPPorts = [
    8384
  ];

  hardware.opengl = {
      enable = true;
  };
}

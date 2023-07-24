{ config, pkgs, ... }:
{
  services.xserver.dpi = 192;
  hardware.acpilight.enable = true;
  boot.kernelPackages = pkgs.linuxPackages_zen;

  fileSystems."/".options = ["compress=zstd"];

  fileSystems."/home/jonathan/Storage" = {
    device = "//100.94.223.34/public";
    fsType = "cifs";
    options = let
      # this line prevents hanging on network split
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s,uid=1000,gid=100";

    in ["${automount_opts}"];
  };

  fileSystems."/home/jonathan/Videos" = {
    device = "//100.94.223.34/video";
    fsType = "cifs";
    options = let
      # this line prevents hanging on network split
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s,uid=1000,gid=100";

    in ["${automount_opts}"];
  };


  fileSystems."/home/jonathan/Music" = {
    device = "//100.94.223.34/music";
    fsType = "cifs";
    options = let
      # this line prevents hanging on network split
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s,uid=1000,gid=100";

    in ["${automount_opts}"];
  };

  environment.systemPackages = with pkgs; [
    intel-gpu-tools
    linuxKernel.packages.linux_zen.system76
    system76-firmware
    microcodeIntel
    # podman-compose
  ];

  hardware.system76.enableAll = true;

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


  # virtualisation.podman = {
  #   enable = true;
  #   autoPrune.enable = true;
  #   dockerCompat = true;

  #   # Required for containers under podman-compose to be able to talk to each other.
  #   defaultNetwork.settings.dns_enabled = true;
  # };

}

{ config, pkgs, ... }:
{
  services.xserver.dpi = 192;
  hardware.acpilight.enable = true;
  boot.kernelPackages = pkgs.linuxPackages_zen;

  fileSystems."/".options = ["compress=zstd"];
  environment.systemPackages = with pkgs; [
    intel-gpu-tools
    linuxKernel.packages.linux_zen.system76
    system76-firmware
    microcodeIntel
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

  fonts.fontconfig = {
      enable = true;
      hinting.enable = true;
      hinting.style = "hintmedium";
  };
}

{ config, pkgs, ... }:
{

      # Enable sound.
      sound.enable = true;
      services = {
      # Enable the X11 windowing system.
      xserver.enable = true;

      # Enable the GNOME Desktop Environment.
      # services.xserver.displayManager.gdm.enable = true;
      xserver.windowManager.awesome.enable = true;
      xserver.windowManager.xmonad.enable = true;
      xserver.windowManager.xmonad.enableContribAndExtras = true;

      # Configure keymap in X11
      xserver.layout = "us";
      xserver.xkbOptions = "caps:none";

      # Enable touchpad support (enabled default in most desktopManager).
      xserver.libinput.enable = true;
      xserver.libinput.touchpad.naturalScrolling = true;
      xserver.libinput.touchpad.disableWhileTyping = true;
      xserver.libinput.touchpad.accelSpeed = "0.25";
      xserver.libinput.mouse.naturalScrolling = true;

      # Enable CUPS to print documents.
      printing.enable = true;
      avahi = {
          enable = true;
          publish = {
              enable = true;
              workstation = true;
              userServices = true;
          };
          openFirewall = true;
          nssmdns = true;
      };

      # hardware.pulseaudio.enable = true;
      pipewire.enable = true;
      pipewire.pulse.enable = true;
      pipewire.alsa.enable = true;
      fwupd.enable = true;
      # services.locate.enable = true;
      # services.locate.interval = "*:0/15";
      tlp.enable = true;

      # Enable the OpenSSH daemon.
      openssh.enable = true;
      httpd.enable = true;
      httpd.adminAddr = "yramagicman@gmail.com";
      httpd.enablePHP = true;
      mysql.package = pkgs.mariadb;
      mysql.enable = true;


      phpfpm.phpOptions = ''
          memory_limit = 2048M
      '';

      redshift.enable = true;
      geoclue2.enable = true;
      fstrim.enable = true;
      atd.enable = true;
  };

  virtualisation.libvirtd.enable = true;

  location.provider = "geoclue2";
}

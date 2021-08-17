
  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.lightdm.greeters.enso.enable = true;
  services.xserver.windowManager.awesome.enable = true;
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;

  # Configure keymap in X11
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "caps:none";

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;
  services.xserver.libinput.touchpad.naturalScrolling = true;
  services.xserver.libinput.touchpad.disableWhileTyping = true;
  services.xserver.libinput.touchpad.accelSpeed = "0.25";
  services.xserver.libinput.mouse.naturalScrolling = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  # hardware.pulseaudio.enable = true;
  services.pipewire.enable = true;
  services.pipewire.pulse.enable = true;
  services.pipewire.alsa.enable = true;
  services.fwupd.enable = true;
  services.locate.enable = true;
  services.tlp.enable = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.httpd.enable = true;
  services.httpd.adminAddr = "yramagicman@gmail.com";
  services.httpd.enablePHP = true;
  services.mysql.package = pkgs.mariadb;
  services.mysql.enable = true;


{ config, pkgs, ... }:
{
  services.xserver = {
    enable = true;

    # Enable the GNOME Desktop Environment.
    displayManager.lightdm.enable = true;
    # desktopManager.gnome.enable = true;
    displayManager.gdm.autoSuspend= false;
    # displayManager.sddm.theme = "elarun";
    displayManager.defaultSession = "none+xmonad";
    displayManager.hiddenUsers =[ "jonathan_backup" ];
    # windowManager.awesome.enable = true;
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;

    # Configure keymap in X11
    layout = "us";
    xkbOptions = "caps:none";

    # Enable touchpad support (enabled default in most desktopManager).
    libinput.enable = true;
    libinput.touchpad.naturalScrolling = true;
    libinput.touchpad.disableWhileTyping = true;
    libinput.touchpad.accelSpeed = "0.25";
    libinput.mouse.naturalScrolling = true;
  };
}

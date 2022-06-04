{ config, pkgs, ... }:
{

    # Enable sound.
    sound.enable = true;

    services = {
        yubikey-agent.enable = true;
        # Enable the X11 windowing system.
        xserver = {
            enable = true;

            # Enable the GNOME Desktop Environment.
            displayManager.sddm.enable = true;
            displayManager.sddm.theme = "elarun";
            displayManager.defaultSession = "none+xmonad";
            displayManager.hiddenUsers =[ "jonathan_backup" ];
            windowManager.awesome.enable = true;
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

        # httpd.enable = true;
        # httpd.adminAddr = "yramagicman@gmail.com";
        # httpd.enablePHP = true;
        # mysql.package = pkgs.mariadb;
        # mysql.enable = true;

        phpfpm.phpOptions = ''
            memory_limit = 2048M
        '';

        redshift.enable = true;
        geoclue2.enable = true;
        fstrim.enable = true;
        atd.enable = true;
        netdata.enable = true;

    };
    networking.firewall.allowedTCPPorts = [
        19999
    ];

    virtualisation.libvirtd.enable = true;

    location.provider = "geoclue2";
    hardware.logitech.wireless.enable = true;
    hardware.logitech.wireless.enableGraphical = true;
    networking.nameservers = [
            "208.67.222.123"
            "208.67.220.123"
        ];


}

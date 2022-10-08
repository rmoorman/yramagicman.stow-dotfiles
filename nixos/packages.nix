{ config, pkgs, ... }:
{
    nixpkgs.config.allowUnfree = true;
    programs.zsh.enable = true;
    environment.systemPackages = with pkgs; [
        apacheHttpd
        file
        gcc
        git
        gnumake
        guvcview
        man-pages
        killall
        mariadb
        nfs-utils
        nodejs
        openssl
        pinentry-gtk2
        python3Full
        racket
        rsync
        unzip
        urlview
        vimHugeX
        w3m
        wget
        xscreensaver
        zsh
    ];

    # Some programs need SUID wrappers, can be configured further or are
    # started in user sessions.
    programs.gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
    };
}

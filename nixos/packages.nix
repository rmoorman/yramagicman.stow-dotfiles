{ config, pkgs, ... }:
{
    nixpkgs.config.allowUnfree = true;
    programs.zsh.enable = true;
    environment.systemPackages = with pkgs; [
        apacheHttpd
        cryptsetup
        file
        gcc
        git
        gnumake
        haskellPackages.xmobar
        home-manager
        htop
        killall
        man-pages
        mariadb
        mu
        neovim
        nextcloud-client
        nfs-utils
        nodejs
        openssl
        pinentry-gtk2
        powertop
        python3Full
        racket
        rsync
        rxvt-unicode
        tmux
        unzip
        vim
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

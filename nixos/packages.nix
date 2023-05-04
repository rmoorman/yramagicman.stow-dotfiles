{ config, pkgs, ... }:
{
    nixpkgs.config.allowUnfree = true;
    programs.zsh.enable = true;
    environment.systemPackages = with pkgs; [
        apacheHttpd
        # cachix
        cryptsetup
        file
        gcc
        git
        gnumake
        home-manager
        htop
        killall
        libva
        man-pages
        mariadb
        neovim
        nfs-utils
        nodejs
        nvme-cli
        nvtop
        openssl
        pinentry-gtk2
        powertop
        python3Full
        racket
        rsync
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

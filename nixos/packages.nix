{ config, pkgs, ... }:
{
    nixpkgs.config.allowUnfree = true;
    programs.zsh.enable = true;
    environment.systemPackages = with pkgs; [
        alacritty
        apacheHttpd
        arandr
        bind
        breeze-icons
        btop
        cmus
        dmenu
        dunst
        dzen2
        ed
        emacs
        feh
        ffmpeg
        file
        firefox
        fzf
        gcc
        ghostscript
        git
        gnome.adwaita-icon-theme
        gnome.aisleriot
        gnome.gedit
        gnumake
        haskellPackages.xmobar
        home-manager
        htop
        iconpack-obsidian
        isync
        killall
        libnotify
        lxappearance
        maim
        man-pages
        mariadb
        mpv
        msmtp
        mu
        mutt
        neovim
        nfs-utils
        nodejs
        openssl
        pass
        pavucontrol
        pcmanfm
        picom
        pinentry-gtk2
        pulsemixer
        pwgen
        python3Full
        racket
        rsync
        rxvt-unicode
        signal-desktop
        silver-searcher
        solaar
        theme-obsidian2
        tmux
        ungoogled-chromium
        universal-ctags
        unzip
        urlview
        vimHugeX
        virt-manager
        vlc
        w3m
        wget
        xclip
        # xonotic
        xorg.xcursorthemes
        xorg.xkill
        xorg.xmessage
        xscreensaver
        zathura
        zsh
    ];

# Some programs need SUID wrappers, can be configured further or are
# started in user sessions.
programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
};

}

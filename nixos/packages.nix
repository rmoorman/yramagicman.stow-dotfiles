{ config, pkgs, ... }:
let
    myPhp = pkgs.php81.withExtensions ({ enabled, all }: enabled ++ [
        all.xdebug
]); in {
    nixpkgs.config.allowUnfree = true;
    programs.zsh.enable = true;
    environment.systemPackages = with pkgs; [
        ag
        alacritty
        apacheHttpd
        arandr
        bind
        breeze-icons
        dmenu
        dunst
        dzen2
        ed
        emacs
        feh
        ffmpeg
        firefox
        fzf
        gcc
        git
        gnome.adwaita-icon-theme
        gnome.aisleriot
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
        mariadb
        mpv
        msmtp
        mu
        mutt
        myPhp
        neovim
        nodejs
        openssl
        pass
        pavucontrol
        pcmanfm
        php81Packages.composer
        php81Packages.phpcbf
        php81Packages.phpcs
        picom
        pinentry-gtk2
        pwgen
        python3Full
        racket
        rsync
        rxvt-unicode
        signal-desktop
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

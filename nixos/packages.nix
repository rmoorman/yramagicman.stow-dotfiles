{ config, pkgs, ... }:
let
    myPhp = pkgs.php.withExtensions ({ enabled, all }: enabled ++ [ all.xdebug
]); in {
    nixpkgs.config.allowUnfree = true;
    programs.zsh.enable = true;
    environment.systemPackages = with pkgs; [
        ag
        alacritty
        apacheHttpd
        breeze-icons
        chromium
        dropbox-cli
        dunst
        dzen2
        emacs
        feh
        firefox
        fzf
        gcc
        git
        gnome.adwaita-icon-theme
        gnumake
        home-manager
        htop
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
        nodejs
        openssl
        pass
        pavucontrol
        pcmanfm
        myPhp
        php74Packages.composer
        php74Packages.phpcbf
        php74Packages.phpcs
        picom
        pinentry-gtk2
        python3Full
        racket
        redshift
        rofi
        rsync
        rxvt-unicode
        signal-desktop
        theme-obsidian2
        tmux
        universal-ctags
        urlview
        vimHugeX
        vlc
        w3m
        wget
        xclip
        xorg.xcursorthemes
        xscreensaver
        zsh
    ];

# Some programs need SUID wrappers, can be configured further or are
# started in user sessions.
programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
};


users.extraGroups.vboxusers.members = [ "jonathan" ];
virtualisation.virtualbox.host.enableExtensionPack = true;
virtualisation.virtualbox.host.enable = true;


}

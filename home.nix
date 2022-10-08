{ config, pkgs, ... }:
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "jonathan";
  home.homeDirectory = "/home/jonathan";

  home.packages = with pkgs; [
    alacritty
    arandr
    breeze-icons
    btop
    calibre
    cmus
    dmenu
    dunst
    dzen2
    emacs
    feh
    ffmpeg
    firefox
    fzf
    ghostscript
    git
    gnome.adwaita-icon-theme
    gnome.aisleriot
    gnome-text-editor
    guvcview
    htop
    iconpack-obsidian
    isync
    libnotify
    lxappearance
    maim
    mpv
    msmtp
    mu
    mutt
    neovim
    nextcloud-client
    pass
    pavucontrol
    pcmanfm
    picom
    pulsemixer
    pwgen
    rxvt-unicode
    signal-desktop
    silver-searcher
    slack
    theme-obsidian2
    tmux
    ungoogled-chromium
    universal-ctags
    urlview
    vimHugeX
    vlc
    xclip
    xonotic
    xorg.xcursorthemes
    xorg.xkill
    xorg.xmessage
    yt-dlp
    zathura
  ];

  home.file = {
    ".editorconfig".source = ./root/editorconfig;
    ".hushlogin".source = ./root/hushlogin;
    ".mbsyncrc".source = ./root/mbsyncrc;
    ".urlview".source = ./root/urlview;
    ".xprofile".source = ./root/xprofile;
    ".zshenv".source = ./root/zshenv;
    ".xmonad".source = ./xmonad;
    ".vim".source = ./vim;
    ".local/bin".source = ./bin;
    ".local/systemd".source = ./systemd;
  };

  # Ideally something like this would be in place.
  # let configs = [

  #   "alacritty"
  #   "autostart"
  #   "awesome"
  #   "backups"
  #   # ... Snip
  # ]
  # # ... Snip
  # xdg = builtins.listToAttrs ( map(mInput@{c, configFile, ... }: {name = configFile."${c}".source; value = "./config/${c}";}) configs);

  # For now, this will do.
  xdg=     {
    configFile."alacritty".source = ./config/alacritty;
    configFile."autostart".source = ./config/autostart;
    configFile."awesome".source = ./config/awesome;
    configFile."backups".source = ./config/backups;
    configFile."bash".source = ./config/bash;
    configFile."colors".source = ./config/colors;
    configFile."crypt".source = ./config/crypt;
    configFile."dunst".source = ./config/dunst;
    configFile."dwm".source = ./config/dwm;
    configFile."emacs".source = ./config/emacs;
    configFile."env".source = ./config/env;
    configFile."feh".source = ./config/feh;
    configFile."getmail".source = ./config/getmail;
    configFile."git".source = ./config/git;
    configFile."gits".source = ./config/gits;
    configFile."help".source = ./config/help;
    configFile."mpd".source = ./config/mpd;
    configFile."mpv".source = ./config/mpv;
    configFile."msmtp".source = ./config/msmtp;
    configFile."mutt".source = ./config/mutt;
    configFile."ncmpcpp".source = ./config/ncmpcpp;
    configFile."nixpkgs".source = ./config/nixpkgs;
    configFile."picom".source = ./config/picom;
    configFile."screenlayout".source = ./config/screenlayout;
    configFile."sounds".source = ./config/sounds;
    configFile."systemd".source = ./config/systemd;
    configFile."tmux.d".source = ./config/tmux.d;
    configFile."vifm".source = ./config/vifm;
    configFile."wget".source = ./config/wget;
    configFile."wmcmd".source = ./config/wmcmd;
    configFile."X11".source = ./config/X11;
    configFile."zsh".source = ./config/zsh;
  };

  xdg.userDirs.desktop = "$HOME/";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}

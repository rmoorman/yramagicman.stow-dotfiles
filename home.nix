{ config, pkgs, lib, ... }:
let
  home = "/home/jonathan";
  dotfiles = "${home}/Documents/dots";
  vim = builtins.mapAttrs (f: z: builtins.readFile (builtins.toPath "${dotfiles}/vim/plugin/${f}") )
    ( builtins.readDir (builtins.toPath "${dotfiles}/vim/plugin" ) );
in {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "jonathan";
  home.homeDirectory = home;
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
    # neovim
    pass
    pavucontrol
    pcmanfm
    picom
    pulsemixer
    pwgen
    rxvt-unicode
    signal-desktop
    silver-searcher
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

  programs.neovim = {
    enable = true;
    # extraConfig = builtins.foldl' (a: b: a + b) ( builtins.attrValues vim );
    extraConfig = lib.concatStrings ([ ( builtins.readFile (builtins.toPath "${dotfiles}/vim/vimrc")  )] ++  builtins.attrValues vim );
  };

  home.file."vim/after" = {
    source="${dotfiles}/vim/after";
    target=".config/nvim/after";
    recursive = true;
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

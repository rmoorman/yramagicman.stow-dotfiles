{ config, pkgs, lib, ... }:
let
  home = "/home/jonathan";
  dotfiles = "${home}/Documents/dots";

  vimFiles = [ "colors" "after" "plugin" "autoload" ];

  nvim = map (f:
    {
      name="nvim/${f}";
      value = (builtins.listToAttrs [
        {name="source"; value="${dotfiles}/vim/${f}";}
        {name="target"; value="${home}/.config/nvim/${f}";}
        {name="recursive"; value=true;}
      ]);
    }) vimFiles;

  vim = map (f:
    {
      name="vim/${f}";
      value = (builtins.listToAttrs [
        {name="source"; value="${dotfiles}/vim/${f}";}
        {name="target"; value="${home}/.vim/${f}";}
        {name="recursive"; value=true;}
      ]);
    }) vimFiles;

  packages = with pkgs; [
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
    vlc
    w3m
    xclip
    xonotic
    xorg.xcursorthemes
    xorg.xkill
    xorg.xmessage
    yt-dlp
    zathura
  ];
in {
  home.username = "jonathan";
  home.homeDirectory = home;
  home.packages = packages;

  programs.neovim = {
    enable = true;
    extraConfig =  ( builtins.readFile (builtins.toPath "${dotfiles}/vim/vimrc")  );
  };

  programs.vim = {
    enable = true;
    extraConfig =  builtins.readFile (builtins.toPath "${dotfiles}/vim/vimrc");
    packageConfigurable = pkgs.vimHugeX;
  };

  home.file = builtins.listToAttrs (
      builtins.concatLists [
          nvim
          vim
      ]
  );

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

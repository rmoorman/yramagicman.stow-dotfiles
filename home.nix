{ config, pkgs, lib, ... }:
let
  home = "/home/jonathan";
  dotfiles = "${home}/Documents/dots";

  packages = with pkgs; [
    arandr
    breeze-icons
    btop
    calibre
    chessx
    cmus
    dmenu
    dunst
    dzen2
    emacs
    feh
    ffmpeg
    firefox
    ghostscript
    git
    gnome.adwaita-icon-theme
    gnome.aisleriot
    gnome.nautilus
    gnome-text-editor
    gnubg
    guvcview
    htop
    iconpack-obsidian
    isync
    libnotify
    lxappearance
    maim
    mpv
    msmtp
    mutt
    mypaint
    neovim
    newsflash
    pass
    pavucontrol
    pcmanfm
    picom
    pulsemixer
    pwgen
    rxvt-unicode
    signal-desktop
    silver-searcher
    stockfish
    theme-obsidian2
    thunderbird
    tmux
    ungoogled-chromium
    universal-ctags
    urlview
    vimHugeX
    vlc
    w3m
    wl-clipboard
    xclip
    xonotic
    xorg.xcursorthemes
    xorg.xkill
    xorg.xmessage
    yt-dlp
    zathura
  ];

  vimFiles = [ "colors" "after" "plugin" "autoload" ];
  zshFiles = [ "aliases" "functions" "prompts" "zpkg" ];

  zsh = map (f:
    {
      name="zsh/${f}";
      value = (builtins.listToAttrs [
        {name="source"; value="${dotfiles}/config/zsh/${f}";}
        {name="target"; value="${home}/.config/zsh/${f}";}
        {name="recursive"; value=true;}
      ]);
    }) zshFiles;

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

  bin = [ {
    name="bin";
    value = (builtins.listToAttrs [
      {name="source"; value="${dotfiles}/bin";}
      {name="target"; value="${home}/.local/bin";}
      {name="recursive"; value=true;}
    ]);
  } ];

in {
  home.username = "jonathan";
  home.homeDirectory = home;
  home.packages = packages;

  # programs.neovim = {
  #   enable = true;
  #   extraConfig =  ( builtins.readFile (builtins.toPath "${dotfiles}/vim/vimrc")  );
  # };

  # programs.vim = {
  #   enable = true;
  #   extraConfig =  builtins.readFile (builtins.toPath "${dotfiles}/vim/vimrc");
  #   packageConfigurable = pkgs.vimHugeX;
  # };

  # programs.zsh = {
  #   enable = true;
  #   completionInit = "";
  #   initExtra =  builtins.readFile (
  #     builtins.toPath "${dotfiles}/config/zsh/zshrc"
  #   );
  #   logoutExtra =  builtins.readFile (
  #     builtins.toPath "${dotfiles}/config/zsh/zlogout"
  #   );
  #   loginExtra =  builtins.readFile (
  #     builtins.toPath "${dotfiles}/config/zsh/zlogin"
  #   );
  #   dotDir=".config/zsh";
  #   envExtra =  builtins.readFile (
  #     builtins.toPath "${dotfiles}/root/zshenv"
  #   );
  # };

  # home.file = builtins.listToAttrs (
  #   builtins.concatLists [
  #     bin
  #     nvim
  #     vim
  #     zsh
  #   ]
  # );

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

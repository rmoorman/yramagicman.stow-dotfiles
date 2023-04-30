{ config, pkgs, lib, ... }:
let

  home = "/home/jonathan";
  dotfiles = "${home}/Documents/dots";

  packages = with pkgs; [
    alacritty
    arandr
    breeze-icons
    btop
    nextcloud-client
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
    gnome.gnome-tweaks
    gnome.nautilus
    gnome-text-editor
    gnubg
    grim
    guvcview
    htop
    iconpack-obsidian
    isync
    libnotify
    libsForQt5.polkit-kde-agent
    light
    lxappearance
    maim
    mpv
    msmtp
    mu
    mutt
    # neovim
    # newsflash
    pandoc
    pass
    pavucontrol
    pcmanfm
    picom
    pulsemixer
    pwgen
    rxvt-unicode
    signal-desktop
    silver-searcher
    slurp
    socat
    stockfish
    swaybg
    theme-obsidian2
    thunderbird
    tmux
    ungoogled-chromium
    universal-ctags
    urlview
    # vimHugeX
    vlc
    w3m
    wezterm
    wl-clipboard
    wofi
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

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.permittedInsecurePackages = [
    "python-2.7.18.6" # GNUbg is old
  ];

  home.username = "jonathan";
  home.homeDirectory = home;
  home.packages = packages;

  programs.neovim = {
    enable = true;
    extraConfig =  builtins.readFile (builtins.toPath "${dotfiles}/vim/vimrc");
  };

  programs.vim = {
    enable = true;
    extraConfig =  builtins.readFile (builtins.toPath "${dotfiles}/vim/vimrc");
    packageConfigurable = pkgs.vimHugeX;
  };

  # programs.emacs = {
  #     enable = true;
  #     extraConfig =  builtins.readFile (builtins.toPath "${dotfiles}/config/emacs/init.el");
  #     extraPackages =  epkgs: with epkgs; [
  #         no-littering
  #         projectile
  #         magit
  #         disable-mouse
  #         base16-theme
  #         auto-package-update
  #         evil
  #         evil-collection
  #         evil-escape
  #         evil-numbers
  #         evil-visualstar
  #         evil-commentary
  #         evil-surround
  #         evil-matchit
  #         general
  #         company
  #         lsp-mode
  #         which-key
  #         ivy
  #         org
  #         lua-mode
  #         markdown-mode
  #         php-mode
  #         racket-mode
  #         web-mode
  #         haskell-mode
  #         typescript-mode
  #         nroff-mode
  #         rust-mode
  #         nix-mode
  #         dockerfile-mode
  #         highlight-indent-guides
  #     ];
  # };

  programs.zsh = {
    enable = true;
    completionInit = "";
    initExtra =  builtins.readFile (
      builtins.toPath "${dotfiles}/config/zsh/zshrc"
    );
    logoutExtra =  builtins.readFile (
      builtins.toPath "${dotfiles}/config/zsh/zlogout"
    );
    loginExtra =  builtins.readFile (
      builtins.toPath "${dotfiles}/config/zsh/zlogin"
    );
    dotDir=".config/zsh";
    envExtra =  builtins.readFile (
      builtins.toPath "${dotfiles}/root/zshenv"
    );
  };

  home.file = builtins.listToAttrs (
    builtins.concatLists [
      bin
      nvim
      vim
      zsh
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

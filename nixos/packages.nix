
  nixpkgs.config.allowUnfree = true;
  programs.zsh.enable = true;
  # List packages installed in system profile. To search, run:
  # programs.mtr.enable = true;
  # $ nix search wget
  environment.systemPackages = with pkgs; [
      ag
      alacritty
      apacheHttpd
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
      php
      php74Packages.composer
      php74Packages.phpcbf
      php74Packages.phpcs
      php74Extensions.xdebug
      picom
      pinentry-gtk2
      python3Full
      racket-minimal
      redshift
      rofi
      rsync
      rxvt-unicode
      signal-desktop
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

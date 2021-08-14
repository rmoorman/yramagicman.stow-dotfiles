
  nixpkgs.config.allowUnfree = true;
  programs.zsh.enable = true;
  # List packages installed in system profile. To search, run:
  # programs.mtr.enable = true;
  # $ nix search wget
  environment.systemPackages = with pkgs; [
      ag
      alacritty
      apacheHttpd
      dunst
      chromium
      dropbox-cli
      dzen2
      emacs
      feh
      firefox
      gcc
      git
      gnumake
      htop
      isync
      killall
      libnotify
      maim
      mariadb
      mpv
      mu
      mutt
      openssl
      pass
      pavucontrol
      php
      php74Packages.composer
      php74Packages.phpcs
      picom
      pinentry-qt
      python3Full
      redshift
      rofi
      rsync
      rxvt-unicode
      signal-desktop
      tmux
      vim
      vlc
      wget
      xclip
      xscreensaver
      zsh
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

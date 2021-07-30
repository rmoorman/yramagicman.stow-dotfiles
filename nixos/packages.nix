
  programs.zsh.enable = true;
  # List packages installed in system profile. To search, run:
  # programs.mtr.enable = true;
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    apacheHttpd
    dzen2
    emacs
    firefox
    git
    htop
    killall
    mariadb
    neovim
    openssl
    pass
    php
    php74Packages.composer
    php74Packages.phpcs
    pinentry-qt
    python3Full
    rsync
    tmux
    vim
    wget
    xclip
    zsh
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

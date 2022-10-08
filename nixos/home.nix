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

  };
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

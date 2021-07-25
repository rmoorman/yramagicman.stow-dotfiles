{
    allowUnfree = true;
    packageOverrides = pkgs: with pkgs; {
        User = pkgs.buildEnv {
            name = "user";
            paths = [
                cantarell-fonts
                dropbox-cli
                feh
                fira-mono
                mu
                mutt
                pavucontrol
                picom
                rofi
                rxvt-unicode
                signal-desktop
                xscreensaver
                ag
            ];
        };
    };
}
#server-searcher

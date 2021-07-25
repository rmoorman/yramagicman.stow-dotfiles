{
    packageOverrides = pkgs : with pkgs; {
        myPackages = pkgs.buildEnv {
            name = "my-packages";
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
                server-searcher
            ];
        };
    };
}

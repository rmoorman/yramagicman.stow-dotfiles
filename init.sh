reposdir="$HOME/Documents"
if test -z "$1"; then
    while read line; do
        printf "%s\n" "$line"
    done <<-EOF
-b) Bin
-c) Config files
-f) Files
-g) Git hooks
-j) Cron Jobs
-n) Nixos
-s) Systemd
-t) Simple terminal
-v) Vim
-w) DWM
-x) Xmonad
-z) ZSH
EOF
fi
if [ $TEST ]; then
    sudo() { echo sudo $@; }
    ln() { echo ln $@; }
    rm() { echo rm $@; }
    cp() { echo ln $@; }
    cd() { echo cd $@; }
    git() { echo git $@; }
    make() { echo make $@; }
    chsh() { echo chsh $@; }
    crontab() { echo crontab $@; }
fi

while test "$1"; do
    case $1 in
        -f) {
            for f in *;
            do
                case $f in
                    *.conf|*.txt|*.sh|*.md|*.h) ;;
                    *) [ -f $f ] &&  ln -sfv "$PWD/$f" "$HOME/.$f" ;;
                esac
            done
        } ;;
        -c) {
            for d in config/*;
            do
                ln -sfv "$PWD/$d" "$HOME/.config/"
            done
        } ;;
        -b) ln -sfv "$PWD/bin" "$HOME/.local/bin" ;;
        -s) ln -sfv "$PWD/systemd" "$HOME/.local/systemd" ;;
        -t) {
            if [ "$(uname)" != "Darwin" ]
            then
                if [ ! -d "$reposdir/st/" ]
                then
                    (
                    cd "$reposdir" || return
                    git clone --depth 3 git://git.suckless.org/st
                    cp "$PWD/st.config.h" "$reposdir/st/config.h"
                    cd "$reposdir/st" || return
                    make
                    sudo make install
                )
            else
                (
                cp "$PWD/st.config.h" "$reposdir/st/config.h"
                cd "$reposdir/st" || return
                make clean
                make
                sudo make install
            )
                fi
            fi
        } ;;
        -w) {
            if [ "$(uname)" != "Darwin" ]
            then
                if [ ! -d "$reposdir/dwm/" ]
                then
                    (
                    cd "$reposdir" || return
                    git clone --depth 3 git://git.suckless.org/dwm
                    cp "$PWD/dwm.config.h" "$reposdir/dwm/config.h"
                    cd "$reposdir/dwm" || return
                    make
                    sudo make install
                )
            else
                (
                cp "$PWD/dwm.config.h" "$reposdir/dwm/config.h"
                cd "$reposdir/dwm" || return
                make clean
                make
                sudo make install
            )
                fi
            fi
        } ;;
        -z) {
            if test "$SHELL" != "$(command -v zsh)"; then
                chsh -s "$(command -v zsh)"
            fi
        } ;;
        -x) ln -sfv "$PWD/xmonad" "$HOME/.xmonad" ;;
        -v) {
            ln -sfv "$PWD/vim" "$HOME/.vim"
            ln -sfv "$PWD/vim" "$HOME/.config/nvim"
        } ;;
        -n) {
            echo "building configuration.nix"
            echo "backing up configuration.nix in place"
            sudo cp -v /etc/nixos/configuration.nix /etc/nixos/configuration.nix.bak
            sudo cp -v "$PWD"/nixos/configuration.nix /etc/nixos/configuration.nix
            echo "generating configuration.nix as /tmp/configuration.nix"
            sudo ln -fs "$PWD"/nixos/header.nix /etc/nixos/header.nix
            sudo ln -fs "$PWD"/nixos/network."$(hostname)".nix /etc/nixos/network.nix
            sudo ln -fs "$PWD"/nixos/packages.nix /etc/nixos/packages.nix
            sudo ln -fs "$PWD"/nixos/fonts.nix /etc/nixos/fonts.nix
            sudo ln -fs "$PWD"/nixos/services.nix /etc/nixos/services.nix
            sudo ln -fs "$PWD"/nixos/users.nix /etc/nixos/users.nix
            sudo ln -fs "$PWD"/nixos/extras."$(hostname)".nix /etc/nixos/extras.nix
            sudo nixos-rebuild boot
        } ;;
        -j) {
            for f in cron/*
            do
                if [ ${f#cron/} == $USER ]; then
                    crontab ${f#cron/}
                fi
                if [ ${f#cron/} == 'root' ]; then
                    sudo crontab ${f#cron/}
                fi
            done
        } ;;
        -g) {
            for f in githooks/*; do
                ln -sfv "$PWD/$f" "$PWD/.git/hooks/${f#githooks/}"
            done
        } ;;
    esac
    shift
done

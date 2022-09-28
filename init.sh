reposdir="$HOME/Documents"
# TEST=1
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
-o) Opt
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
            mkdir -p "$HOME/.config"
            for d in config/*;
            do
                ln -sfv "$PWD/$d" "$HOME/.config/"
            done
        } ;;
        -b) ln -sfv "$PWD/bin" "$HOME/.local/" ;;
        -s) {
            ln -sfv "$PWD/systemd" "$HOME/.local/"
            systemctl --user enable --now battery_notify.path
            systemctl --user enable --now battery_notify.service
            systemctl --user enable --now downloads.path
            systemctl --user enable --now downloads.service
            systemctl --user enable --now mbsync.path
            systemctl --user enable --now mbsync_clean.path
            systemctl --user enable --now battery_file.timer
            systemctl --user enable --now mbsync.timer
            systemctl --user enable --now mu.timer
            systemctl --user enable --now remind.timer
        } ;;
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
        -x) {
            xmonad="$HOME/.xmonad"
            [[ -L "$xmonad" ]] && rm -v $xmonad
            ln -sfv "$PWD/xmonad" $xmonad
        } ;;
        -v) {
            vim="$HOME/.vim"
            nvim="$HOME/.config/nvim"
            [[  -L $vim ]] && rm -v $vim
            ln -sfv "$PWD/vim" $vim
            [[  -L "$nvim" ]] && rm -v $nvim
            ln -sfv "$PWD/vim" $nvim
        } ;;
        -n) {
            if [ -f '/etc/nixos/configuration.nix' ];
            then
                echo "building configuration.nix"
                echo "backing up configuration.nix in place"
                sudo cp -v /etc/nixos/configuration.nix /etc/nixos/configuration.nix.$(date +"%s").bak
            fi
            echo build nix flake from repo
            (cd $(git rev-parse --show-toplevel) && \
                sudo nixos-rebuild boot --flake './nixos#' --upgrade)
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
        -o) {
            [[ -d "/opt" ]] || sudo mkdir /opt

            for f in opt/*; do
                sudo ln -sfv "$PWD/$f" "/opt/${f#opt/}"
            done
        }
    esac
    shift
done

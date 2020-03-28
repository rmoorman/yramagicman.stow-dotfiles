#!/bin/sh
logfile=$HOME/initlog
dotdir="$(dirname "$(realpath "$0")" )"
reposdir="$HOME/Documents"
printf '' > "$logfile"
dotfiles() {
    find ./ -maxdepth 1 -type f  \
        -not -name '.*' -and \
        -not -name 'config' -and \
        -not -name 'license.txt' -and \
        -not -name '*.h' -and \
        -not -name 'cron' -and \
        -not -name 'README.md' -and \
        -not -name 'uninstall.sh' -and \
        -not -name 'init.sh' | while read -r f
        do
            if test ! -f "$HOME/.$(basename "$f" )"
            then
                ln -sfv "$dotdir/$(basename "$f")" "$HOME/.$(basename "$f")"
            else
                printf 'skipped %s\r\n' "$f"
                printf 'Files Not Installed: ' >> "$logfile"
                printf "%s\n" "$f" >> "$logfile"

            fi
        done
    }

dotdirs() {
    find ./ -maxdepth 1 -type d  \
        -not -name '.git' -and \
        -not -name 'config' -and \
        -not -name 'cron' -and \
        -not -name 'githooks' -and \
        -not -name 'bin' | while read -r d
        do
            if test ! -d "$HOME/.$(basename "$d" )"
            then
                ln -sv "$dotdir/$(basename "$d")" "$HOME/.$(basename "$d")"
            else
                printf 'skipped %s\r\n' "$d"
                printf '\nDot Directories Not Installed: ' >> "$logfile"
                printf "%s\n" "$d" >> "$logfile"

            fi
        done
    }

configdir() {
    if test ! -d "$HOME/.config"
    then
        echo "making ~/.config directory"
        mkdir -p "$HOME/.config"
    fi
    find "$dotdir/config/" -maxdepth 1 \
        -not -name 'config' \
        -not -name 'nvim'  | while read -r directory
        do
            if test ! -d "$HOME/.config/$(basename "$directory" )" && test ! -f "$HOME/.config/$(basename "$directory")"
            then
                ln -sv "$directory" "$HOME/.config/"
            else
                printf 'skipped %s\r\n' "$directory"
                printf '\nConfig files Not Installed: ' >> "$logfile"
                printf "%s\n" "$directory" >> "$logfile"

            fi
        done
    }

githooks() {
    for f in githooks/*; do
        ln -sf "$dotdir/$f" "$dotdir/.git/hooks/$(basename "$f")"
    done
}

bindir() {
    if test ! -d "$HOME/bin"
    then
        ln -sfv "$dotdir/bin" "$HOME/"
    else
        printf 'skipped %s\r\n' "bin"
        printf '\nMisc files Not Installed: ' >> "$logfile"
        printf "%s\n" "$dotdir/bin" >> "$logfile"
    fi
}

mkrepodir() {
    if test ! -d "$reposdir"
    then
        mkdir -p "$reposdir"
    fi
}

st() {
    if test "$(uname)" != "Darwin"
    then
        if test ! -d "$reposdir/st/"
        then
            (
            cd "$reposdir" || return
            git clone --depth 3 git://git.suckless.org/st
            cp "$dotdir/st.config.h" "$reposdir/st/config.h"
            cd "$reposdir/st" || return
            make
            sudo make install
        )
    else
        (
        cp "$dotdir/st.config.h" "$reposdir/st/config.h"
        cd "$reposdir/st" || return
        make
        sudo make install
    )
        fi
    fi
}

dwm() {
    if test "$(uname)" != "Darwin"
    then
        if test ! -d "$reposdir/dwm/"
        then
            (
            cd "$reposdir" || return
            git clone --depth 3 git://git.suckless.org/dwm
            cp "$dotdir/dwm.config.h" "$reposdir/dwm/config.h"
            cd "$reposdir/dwm" || return
            make
            sudo make install
        )
    else
        (
        cp "$dotdir/dwm.config.h" "$HOME/Gits/dwm/config.h"
        cd "$reposdir/dwm" || return
        make
        sudo make install
    )
        fi
    fi
}

setshell() {
    if test "$SHELL" != "$(command -v zsh)"; then
        chsh -s "$(command -v zsh)"
    fi
}

cron() {
    find "$dotdir/cron" -type f | while read -r job
do
    j="$(basename "$job" )"
    if test "$j" == 'root'
    then
        echo installing cron for "$j"
        sudo crontab "$job"
    else
        if test "$(who | cut -d ' ' -f 1 | sed 1q)" == "$j"
        then
            echo installing cron for "$j"
            crontab "$job"
        fi
    fi
done
}

args="$(getopt fdcbrtwsja "$@")"
if test -z "$1"
then
    cat <<EOF
-f) Dot files
-d) Dot directoriess
-c) Config directories
-b) Bin directory
-r) Make repos directory
-t) Build st
-w) Build dwm
-s) Set shell
-j) Install cron jobs
-a) Installs everything
EOF
fi
set -- $args
for i
do
    case "$i"
        in
        -f)
        dotfiles
        shift;;
    -d)
        dotdirs
        shift;;
    -c)
        configdir
        shift;;
    -b)
        bindir
        shift;;
    -r)
        mkrepodir
        shift;;
    -t)
        st
        shift;;
    -w)
        dwm
        shift;;
    -s)
        setshell
        shift;;
    -j)
        cron
        shift;;
    -a)
        bindir
        configdir
        cron
        dotdirs
        dotfiles
        dwm
        mkrepodir
        setshell
        st
        shift;;
    --)
        githooks
        shift;
        break;;
esac
done

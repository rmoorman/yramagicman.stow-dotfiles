#!/bin/sh
logfile=$HOME/initlog
dotdir="$(dirname "$(realpath "$0")")"
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
        -not -name '*.ignore' -and \
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
        -not -name 'ignore' -and \
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
        set_nvim
    }

set_nvim() {
    if test ! -d "$HOME/.config/nvim"
    then
        ln -s "$dotdir/vim" "$HOME/.config/nvim"
    fi
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
    if test ! -d "$HOME/.local/bin"
    then
        ln -sfv "$dotdir/bin" "$HOME/.local/"
    else
        printf 'skipped %s\r\n' "bin"
        printf '\nMisc files Not Installed: ' >> "$logfile"
        printf "%s\n" "$dotdir/.local/bin" >> "$logfile"
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
        make clean
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
        cp "$dotdir/dwm.config.h" "$reposdir/dwm/config.h"
        cd "$reposdir/dwm" || return
        make clean
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

_linkfiles() {
    find "$1" -type f | while read -r file
do
    echo 'linking into '"$2"
    sudo ln -fs "$file" /etc/"$2"/"$(basename "$file")"
done
}

cron() {
    find  "$dotdir/cron" -maxdepth 1 -type f | while read -r job
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

find  "$dotdir/cron" -maxdepth 1 -type d | while read -r job
do

    case "$(basename "$job" )" in
        'daily' )
            _linkfiles "$job"  'cron.daily';;
        'weekly' )
            _linkfiles "$job" 'cron.weekly';;
        'monthly' )
            _linkfiles "$job" 'cron.monthly';;
    esac
done
}

clean() {
    if test "$( wc -l < "$logfile" )" == 0; then
        rm "$logfile"
    fi
}

my_nix() {
    echo "building configuration.nix"
    echo "backing up configuration.nix in place"
    sudo cp -v /etc/nixos/configuration.nix /etc/nixos/configuration.nix.bak
    sudo cp -v "$dotdir"/nixos/configuration.nix /etc/nixos/configuration.nix
    echo "generating configuration.nix as /tmp/configuration.nix"

    sudo ln -s "$dotdir"/nixos/header.nix /etc/nixos/header.nix
    sudo ln -s "$dotdir"/nixos/network."$(hostname)".nix /etc/nixos/network.nix
    sudo ln -s "$dotdir"/nixos/packages.nix /etc/nixos/packages.nix
    sudo ln -s "$dotdir"/nixos/fonts.nix /etc/nixos/fonts.nix
    sudo ln -s "$dotdir"/nixos/services.nix /etc/nixos/services.nix
    sudo ln -s "$dotdir"/nixos/users.nix /etc/nixos/users.nix
    sudo ln -s "$dotdir"/nixos/extras."$(hostname)".nix /etc/nixos/extras.nix
    sudo ln -s "$dotdir"/nixos/footer.nix /etc/nixos/footer.nix

    echo 'regen how? (b)oot, (v)m, (t)est, (i)n place, (u)pgrade, anything else to skip'
    read inplace
    if test "$inplace" == 'b'; then
        sudo nixos-rebuild boot
    elif test "$inplace" == 'v'; then
        sudo nixos-rebuild build-vm
    elif test "$inplace" == 't'; then
        sudo nixos-rebuild test
    elif test "$inplace" == 'i'; then
        sudo nixos-rebuild switch
    elif test "$inplace" == 'u'; then
        sudo nixos-rebuild boot --upgrade
        nix-collect-garbage
        nix optimise-store
    else
        return
    fi
}

if test -z "$1"; then
    while read line; do
        printf "%s\n" "$line"
    done <<-EOF
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
    -n) Build nix config from files
    -v) install nvim directory
EOF
fi
while test "$1"; do
    case $1 in
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
        -v)
            set_nvim
            shift;;
        -n)
            my_nix
            shift;;
        -a)
            bindir
            configdir
            my_nix
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
            clean
            shift;
            break;;
    esac
done

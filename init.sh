#!/bin/sh

dotdir="$(dirname "$(realpath "$0")" )"
find ./ -maxdepth 1 -type f  \
    -not -name '.*' -and \
    -not -name 'config' -and \
    -not -name 'license.txt' -and \
    -not -name '*.h' -and \
    -not -name 'README.md' -and \
    -not -name 'uninstall.sh' -and \
    -not -name 'init.sh' | while read -r f
do
    ln -sv "$dotdir/$(basename "$f")" "$HOME/.$(basename "$f")"
done
crontab "$dotdir/joncron"
find ./ -maxdepth 1 -type d  \
    -not -name '.git' -and \
    -not -name 'config' -and \
    -not -name 'bin' | while read -r d
do
    if test ! -d "$HOME/.$(basename "$d" )"
    then
        ln -sv "$PWD/$(basename "$d")" "$HOME/.$(basename "$d")"
    fi
done

if test ! -d "$HOME/.config"
then
    echo "making ~/.config directory"
    mkdir -p "$HOME/.config"
fi
find "$PWD/config/" -maxdepth 1 -not -name 'config' | while read -r directory
do
    ln -sv "$directory" "$HOME/.config/"
done
ln -sv "$PWD/bin" "$HOME/"

if test ! -d "$HOME/Gits"
then
    mkdir -p "$HOME/Gits"
fi
if test ! -d "$HOME/Gits/st/"
then
    cd "$HOME/Gits" || return
    git clone --depth 3 git://git.suckless.org/st
    ln -s "$dotdir/st.config.h" "$HOME/Gits/st/config.h"
    cd "$HOME/Gits/st" || return
    make
    sudo make install
fi
if test ! -d "/home/jonathan/Gits/dwm/"
then
    cd "$HOME/Gits" || return
    git clone --depth 3 git://git.suckless.org/dwm
    ln -s "$dotdir/dwm.config.h" "$HOME/Gits/dwm/config.h"
    cd "$HOME/Gits/dwm" || return
    make
    sudo make install
fi
chsh -s "$(command -v zsh)"

#!/bin/sh

if ! type "stow" > /dev/null
then
    if type "pacman" >/dev/null
    then
        echo "On Arch, installing stow automatically"
        sudo pacman -S stow
        else
            echo "Please install GNU Stow"

            return
        fi
    else
        echo ''
    fi

    find ~/ -maxdepth 1 -type f -iname '.*' | while read -r f
    do
        if test -f "./root/$(basename "$f")"
        then
            echo "Backing up $f"
            mv -v "$f" "$f.bak"
        fi
    done

    for f in $(find ./ -maxdepth 1 -type d | grep -Ev '.git$|config|zsh' | cut -d '/' -f 2 )
    do
        stow -R "$f" -t ~/
    done

    if test ! -d "$HOME/.config"
    then
        echo "making ~/.config directory"
        mkdir -p "$HOME/.config"
    fi
    stow -R config -t ~/.config

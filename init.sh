#!/bin/sh

find ./ -maxdepth 1 -type f  \
    -not -name '.*' -and \
    -not -name 'config' -and \
    -not -name 'license.txt' -and \
    -not -name 'README.md' \
    -not -name 'init.sh' | while read -r f
do
    ln -sv "$PWD/$(basename "$f")" "$HOME/.$(basename "$f")"
done

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

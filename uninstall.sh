#!/bin/sh

find ./ -maxdepth 1 -type f | while read -r file
do
    if test -f "$HOME/.$(basename "$file")"
    then
        rm -v "$HOME/.$(basename "$file")"
    fi
done

find ./ -maxdepth 1 -type d | while read -r dir
do
    if test -d "$HOME/.$(basename "$dir")"
    then
        rm -v "$HOME/.$(basename "$dir")"
    elif test -d "$HOME/$(basename "$dir")"
    then
        rm -v "$HOME/$(basename "$dir")"
    fi
done

find ./config/ -maxdepth 1  | while read -r config
do
    if test -f "$HOME/.config/$(basename "$config")"
    then
        rm -v "$HOME/.config/$(basename "$config")"
    elif test -d "$HOME/.config/$(basename "$config")"
    then
        rm -v "$HOME/.config/$(basename "$config")"
    fi
done

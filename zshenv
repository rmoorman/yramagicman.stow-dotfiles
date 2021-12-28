#!usr/bin/env zsh
# start xserver when necessary
# if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
#     exec startx "$HOME/.config/X11/xinitrc"
# fi

# set config variables and ensure files are linked
setopt extendedglob
local dotdir="$HOME/.config/zsh"
zmodload -m -F zsh/files b:zf_ln b:zf_rm
for file in $dotdir/^.*; do
    if [[ ! -f "$dotdir/.${file:t}" ]] && [[ -f "$file" ]]
    then
        [[ -f $file ]] && zf_ln -s "$file" "$dotdir/.${file:t}"
        zf_rm -rf .pkg .zsh .plugins
    else
        ZDOTDIR="$dotdir"
    fi
done
zmodload -u zsh/files

source $HOME/.config/env
# Paths

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

# Set the list of directories that Zsh searches for programs.
path=(
    $HOME/.local/bin
    $path
)

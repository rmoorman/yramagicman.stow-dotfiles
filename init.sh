#!/bin/sh
logfile=$HOME/initlog
printf '' > "$logfile"
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
    if test ! -f "$HOME/.$(basename "$f" )"
    then
        ln -sv "$dotdir/$(basename "$f")" "$HOME/.$(basename "$f")"
    else
        printf 'Files Not Installed: ' >> "$logfile"
        printf "%s\n" "$f" >> "$logfile"

    fi
done
find ./ -maxdepth 1 -type d  \
    -not -name '.git' -and \
    -not -name 'config' -and \
    -not -name 'githooks' -and \
    -not -name 'bin' | while read -r d
do
    if test ! -d "$HOME/.$(basename "$d" )"
    then
        ln -sv "$PWD/$(basename "$d")" "$HOME/.$(basename "$d")"
    else
        printf '\nDot Directories Not Installed: ' >> "$logfile"
        printf "%s\n" "$d" >> "$logfile"

    fi
done

if test ! -d "$HOME/.config"
then
    echo "making ~/.config directory"
    mkdir -p "$HOME/.config"
fi
find "$PWD/config/" -maxdepth 1 -not -name 'config' | while read -r directory
do
    if test ! -d "$HOME/.config/$(basename "$directory" )" && test ! -f "$HOME/.config/$(basename "$directory")"
    then
        ln -sv "$directory" "$HOME/.config/"
    else
        printf '\nConfig files Not Installed: ' >> "$logfile"
        printf "%s\n" "$directory" >> "$logfile"

    fi
done

for f in githooks/*; do
    ln -sf "$dotdir/$f" "$dotdir/.git/hooks/$(basename "$f")"
done

if test ! -d "$HOME/bin"
then
    ln -sv "$dotdir/bin" "$HOME/"
else
    printf '\nMisc files Not Installed: ' >> "$logfile"
    printf "%s\n" "$dotdir/bin" >> "$logfile"
fi

if test ! -d "$HOME/Gits"
then
    mkdir -p "$HOME/Gits"
fi
if test ! -d "$HOME/Gits/st/"
then
    (
    cd "$HOME/Gits" || return
    git clone --depth 3 git://git.suckless.org/st
    cp "$dotdir/st.config.h" "$HOME/Gits/st/config.h"
    cd "$HOME/Gits/st" || return
    make
    sudo make install
)
else
    (
    cp "$dotdir/st.config.h" "$HOME/Gits/st/config.h"
    cd "$HOME/Gits/st" || return
    make
)
fi
if test ! -d "$HOME/Gits/dwm/"
then
    (
    cd "$HOME/Gits" || return
    git clone --depth 3 git://git.suckless.org/dwm
    cp "$dotdir/dwm.config.h" "$HOME/Gits/dwm/config.h"
    cd "$HOME/Gits/dwm" || return
    make
    sudo make install
)
else
    (
    cp "$dotdir/dwm.config.h" "$HOME/Gits/dwm/config.h"
    cd "$HOME/Gits/dwm" || return
    make
)
fi
if test "$SHELL" != "$(command -v zsh)"; then
    chsh -s "$(command -v zsh)"
fi

printf "installing cronjobs for user\n"
crontab "$dotdir/joncron"
printf "installing cronjobs for root\n"
sudo crontab "$dotdir/rootcron"

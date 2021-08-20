#!/bin/sh

#
# ~/.xsession
#
# Executed by xdm/gdm/kdm at login
#


if which dbus-launch >/dev/null && test -z "$DBUS_SESSION_BUS_ADDRESS"; then
    eval "$(dbus-launch --sh-syntax --exit-with-session)"
fi


# Ensure that GTK themes are applied uniformly in the Desktop Environment

export GTK2_RC_FILES="$HOME/.config/gtk-2.0/gtkrc-2.0"


# Where the default Home folders have not already been created, generate them

if [ -x /usr/bin/xdg-user-dirs-update ]; then
    /usr/bin/xdg-user-dirs-update
fi



userresources=$HOME/.config/X11/Xresources
usermodmap=$HOME/.config/X11/Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then

    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then

    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

if test "$( hostname )" == 'tightpants'; then
    echo 'Xft.dpi: 192' | xrdb -override
fi

. "$HOME/.config/env"
if xrandr --listactivemonitors | grep -c 'HDMI-1'; then
    xrandr --output eDP-1 --off
fi
export GTK2_RC_FILES="$HOME/.config/gtk-2.0/gtkrc-2.0"
export PATH="/home/jonathan/.local/bin:$PATH"
bash --login -i $HOME/.config/X11/xinitrc

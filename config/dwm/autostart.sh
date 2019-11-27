#!/bin/sh
xbacklight -set 25
setxkbmap -option compose:menu
setxkbmap -option caps:none
if test "$( hostname )" = "k-nine" || test "$( hostname )" = "serenity"
then
    "$HOME/bin/setmouse" &
fi
check_process(){

    if  ! pgrep "$1";
    then
        "$@" &
    fi

}

set_screen_layout() {

    ( /home/jonathan/.screenlayout/default.sh ) &
    if test "$( hostname )" == 'serenity'; then
        amixer -c 0  --  set  Master  0
        if test "$(xrandr | awk '/HDMI1/ {print $2}' )" == 'connected'; then

            ( /home/jonathan/.screenlayout/work.sh ) &
        fi
    fi
    if test "$( hostname )" == 'artoo'; then
        echo 'Xft.dpi: 192' | xrdb -override
    fi
}

set_screen_layout
"$HOME/bin/statusloop" &
("$HOME/.config/dwm/scripts/bg.sh") &

xset -dpms; xset s off &

(sleep 1s && check_process compton -b)

(sleep 1s && /usr/bin/xscreensaver -no-splash) &
(sleep 5s  && "$HOME/bin/get_remote_ip") &
(sleep 5s  && emacs --bg-daemon) &

## Set keyboard settings - 250 ms delay and 25 cps (characters per
## second) repeat rate.  Adjust the values according to your
## preferances.
xset r rate 250 25 &

dunst &

pulseaudio &
## Turn on/off system beep
xset b off &

if test "$(hostname)" != 'artoo'; then
    # Autostart the Dropbox deamon
    (sleep 100s && dropbox-cli start) &
fi
(sleep 45s && check_process redshift) &


#limit the size of dirs history
(
d=$(sort -u  "$HOME/.cache/zsh/dirs" )
rm "$HOME/.cache/zsh/dirs"
echo "$d" > "$HOME/.cache/zsh/dirs"
) &

(
h=$(sort -u  "$HOME/.surf/history.txt" )
rm "$HOME/.surf/history.txt"
echo "$h" > "$HOME/.surf/history.txt"
) &
# make sure tmux digests file isn't overly large
(
z=$(tail  "$HOME/.tmux.d/digests" -n "$(find  "$HOME/.tmux.d/" | wc -l)" )
rm "$HOME/.tmux.d/digests"
echo "$z" > "$HOME/.tmux.d/digests"
) &

(
echo "" > "$HOME/.xsession-errors"
if ! stat "$HOME/.xsession-errors.old" > /dev/null; then
    rm "$HOME/.xsession-errors.old"
fi

if  stat "$HOME/.cache/updates" > /dev/null; then
    rm "$HOME/.cache/updates"
fi
) &


(
echo '' > "$HOME/.config/fetchmail.log"
echo '' > "$HOME/.config/procmail.log"
echo '' > "$HOME/.config/msmtp.log"
echo '' > "$HOME/.mpd/mpdstate"
rm "$HOME/slacklogs"
rm "$HOME/nohup"
) &

exit

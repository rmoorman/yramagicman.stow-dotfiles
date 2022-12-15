#!/usr/bin/env zsh

#{{{ > Easier navigation: .., ..., ...., ....., ~ and -
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ~="cd ~"
alias d="dirs -v"
#}}}
#{{{ shortcuts
alias mutt=" mutt -F $HOME/.config/mutt/muttrc"
alias m="mutt"
alias h="history"
alias py="python"
alias .vim="cd $HOME/.vim && ls"
alias push="git push -u origin HEAD"
alias pull="git pull --rebase"
alias j="jobs"
if [[ $INSIDE_EMACS ]]; then
    alias vim="ec"
    alias vi="ec"
    alias v="ec"
    alias v.="ec ."
    alias gvim="ec"
else
    alias nvim="firejail nvim"
    alias vim="nvim"
    alias vi="nvim"
    alias v="nvim"
    alias v.="nvim ."
    alias gnvim="vim"
fi
alias oo="nohup xdg-open . > /dev/null &"

function open() {
    for elem in $@
    do
        nohup xdg-open $elem > /dev/null &
    done
}

function vimrc() {
    vim -c ':e $MYVIMRC'
}

alias t='tmux'
alias :q='exit'
alias bc='bc -l'
alias getmail="$HOME/.local/bin/getallmail"
alias remacs="emacsclient --eval '(kill-emacs)' && emacs --bg-daemon"
if [[ $( command -v doas ) ]]; then
    alias sudo='doas '
else
    alias sudo='sudo '
fi
alias rm="rm -rv"
#}}}
#{{{ web dev stuff
alias speedtest="wget -O /dev/null http://speedtest.wdc01.softlayer.com/downloads/test10.zip"
alias wget="wget -c --no-check-certificate"
alias artisan="php artisan"
#}}}
#{{{ colorize stuff, utility commands
#{{{ Detect which `ls` flavor is in use
if ls --color >/dev/null 2>&1; then # GNU `ls`
    colorflag="--color=auto"
else # OS X `ls`
    colorflag="-G"
fi
if [[ "$INSIDE_EMACS" ]]; then
    colorflag=""
fi
#}}}
#{{{ pretty ls
# List all files colorized in long format
alias l="ls -Flh ${colorflag}"
alias ls-a="ls -Fah ${colorflag}"
# List all files colorized in long format, including dot files
alias la="ls -Flah ${colorflag}"
alias ll="ls -Flh ${colorflag}"
# List only directories
alias lsd='ls -Flh ${colorflag} | grep "^d"'
# Always use color output for `ls`
alias ls="command ls -Fh ${colorflag}"
#}}}
alias grep="command grep --color=auto"
#}}}
#{{{ volume control
alias maxvol="amixer -c 0 -- set Master 100%"
alias headphones="amixer -c 0 -- set Master 45%"
alias mute="amixer -c 0 -- set Master mute"
alias unmute="amixer -c 0 -- set Master on"
#}}}
#{{{ network
# Gzip-enabled `curl`
alias curl="curl -L --compressed"
# Enhanced WHOIS lookups
alias whois="whois -h whois-servers.net"
alias rip="mdig +short myip.opendns.com @resolver1.opendns.com"
alias ip="ip --color=auto"
function nsync() {
    nextcloudcmd --user jonathan --password $(pass show nextcloud) $HOME/Nextcloud http://100.94.223.34
}
#}}}
# {{{ utilities
# Ring the terminal bell
alias bell="tput bel;cvlc --play-and-exit $HOME/.config/sounds/beep.mp3 2> /dev/null"
alias wttr="curl wttr.in"
function mpv() {
    [[ -e /usr/bin/mpv ]] && {
        /usr/bin/mpv "$@"
        return
    }
    command mpv "$@"
    return
}
#}}}
#{{{ tmux
alias tmux="tmux -f $XDG_CONFIG_HOME/tmux.d/tmux.conf"
alias tkill="tmux kill-session -t"
alias tkills="tmux kill-server"
alias tns="tmux new-session"
alias tls="tmux ls"
function trun() {
    wind_name="cmd_$RANDOM"
    tmux new-window -t "$HOSTNAME" -c "$PWD" -n "$wind_name"
    tmux send-keys -t "$HOSTNAME:$win_name" "cd $PWD && $@ && bell && read && exit" Enter
}
#}}}
#{{{ system management aliases
if  [[ -z $SSH_CLIENT && $(command -v systemctl) ]]; then
    alias poweroff="systemctl poweroff"
    alias reboot="systemctl reboot"
    alias hibernate="systemctl suspend"
elif [[ $(command -v systemctl) ]]; then
    alias poweroff="sudo systemctl poweroff"
    alias reboot="sudo systemctl reboot"
    alias hibernate="sudo systemctl suspend"
else
    alias poweroff="sudo poweroff"
    alias reboot="sudo reboot"
fi
if [[ $(command -v yay) ]]; then
    alias pacman="yay"
fi
#}}}
#{{{ utility commands
if [[ $(command -v nix-shell) ]]; then
    alias sl="nix-shell -p sl --run sl"
fi
alias q="exit"
alias mypw="pwgen -c -n -s -y 26 -1"
alias ndate="date \"+%d-%m-%y\""
# easy reload of zsh stuff
alias rl="exec zsh -l"
alias zconfig="$EDITOR $ZDOTDIR/zshrc"
alias x="startx ~/.config/X11/xinitrc"
#}}}
#{{{ git configs
alias g="git"
alias gca="git commit --all --verbose"
alias gco="git checkout"
alias gs="git status --short"
#}}}
# {{{ docker
alias dc='docker-compose'
alias drc='docker-compose exec'
alias dupd='docker-compose up -d'
alias dupbd='docker-compose up -d --build'
alias dud='docker-compose down'
#}}}
#{{{ NFS store mounts
alias musicmount="sudo mount -t nfs  192.168.1.195:/music /home/jonathan/Music"
alias vidmount="sudo mount -t nfs  192.168.1.195:/video /home/jonathan/Videos"
alias storemount="sudo mount -t nfs  192.168.1.195:/storage /home/jonathan/Storage"
#}}}
#{{{ BTRFS du/df things
alias bdu="btrfs filesystem du"
alias bdf="btrfs filesystem df"
#}}}
#{{{ tailscale
function tsup-browncoat() {
    sudo tailscale up \
         --operator=jonathan \
         --reset \
         --exit-node '100.94.223.34' \
         --exit-node-allow-lan-access=true
}

function tsup-fast() {
    sudo tailscale up \
         --operator=jonathan \
         --reset \
         --exit-node '100.87.149.57'

}
alias tsdown="sudo tailscale down"
function tsup-exit() {
    sudo tailscale up \
         --operator=jonathan \
         --advertise-exit-node \
         --accept-routes \
         --advertise-routes=192.168.0.0/24,192.168.1.0/24
}
alias debspeed="wget https://cdimage.debian.org/cdimage/unofficial/non-free/cd-including-firmware/11.5.0-live+nonfree/amd64/iso-hybrid/debian-live-11.5.0-amd64-cinnamon+nonfree.iso -O /dev/null"

#}}}

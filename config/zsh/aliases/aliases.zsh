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
alias mutt="mutt -F $HOME/.config/mutt/muttrc && touch -- $HOME/.config/mail/.modified"
alias m="mutt -F $HOME/.config/mutt/muttrc && touch -- $HOME/.config/mail/.modified"
alias h="history"
alias py="python"
alias .vim="cd $HOME/.vim && ls"
alias push="git push -u origin HEAD"
alias pull="git pull --rebase"
alias j="jobs"
alias vi="vim"
alias vim="vim"
alias v="vim"
alias :e="emacslient -nw ."
alias e="emacsclent -nw"
alias v.="vim ."
alias gvim="vim"
alias ec="emacsclient -nc"
alias ech="emacsclient -n"
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
alias sudo='sudo '
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
#}}}
# {{{ utilities
# Canonical hex dump; some systems have this symlinked
# URL-encode strings
alias urlencode='python2 -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1]);"'
# Ring the terminal bell, and put a badge on Terminal.app’s Dock icon
# (useful when executing time-consuming commands)
alias bell="tput bel;cvlc --play-and-exit $HOME/.config/sounds/beep.mp3 2> /dev/null"
alias wttr="curl wttr.in"
#}}}
#{{{ tmux
alias tmux="tmux -f $XDG_CONFIG_HOME/tmux.d/tmux.conf"
alias tkill="tmux kill-session -t"
alias tkills="tmux kill-server"
alias tns="tmux new-session"
alias tls="tmux ls"
#}}}
#{{{ system management aliases
if  [[ -z $SSH_CLIENT ]] ; then
    alias poweroff="systemctl poweroff"
    alias reboot="systemctl reboot"
    alias hibernate="systemctl suspend"
else
    alias poweroff="sudo systemctl poweroff"
    alias reboot="sudo systemctl reboot"
    alias hibernate="sudo systemctl suspend"
fi
if [[ $(command -v yay) ]]; then
    alias pacman="yay"
fi
#}}}
#{{{ utility commands
alias sl="nix-shell -p sl --run sl"
alias q="exit"
alias mypw="pwgen -c -n -s -y 16 -1"
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
alias drp='docker-compose exec php-fpm'
alias drc='docker-compose exec'
alias dupd='docker-compose up -d'
alias dupbd='docker-compose up -d --build'
alias dud='docker-compose down'
#}}}
#{{{ NFS store mounts
alias musicmount="sudo mount -t nfs  browncoat:/music /home/jonathan/Music"
alias vidmount="sudo mount -t nfs  browncoat:/video /home/jonathan/Videos"
alias storemount="sudo mount -t nfs  browncoat:/storage /home/jonathan/Storage"
#}}}
#{{{ BTRFS du/df things
alias bdu="btrfs filesystem du"
alias bdf="btrfs filesystem df"
#}}}

#{{{ > Easier navigation: .., ..., ...., ....., ~ and -
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ~="cd ~"
alias d="dirs -v"
#}}}
#{{{ shortcuts
alias mutt="mutt -F $HOME/.config/mutt/muttrc"
alias m="mutt -F $HOME/.config/mutt/muttrc"
alias h="history"
alias py="python"
alias .vim="cd $HOME/.vim && ls"
alias push="git push -u origin HEAD"
alias pull="git pull --rebase"
alias j="jobs"
alias vi="$EDITOR"
alias vim="$EDITOR"
alias v="$EDITOR"
alias :e="$EDITOR ."
alias e="$EDITOR"
alias v.="$EDITOR ."
alias gvim="$EDITOR"
alias oo="nohup xdg-open . > /dev/null &"
alias t='tmux'
alias :q='exit'
alias bc='bc -l'
alias getmail="$HOME/bin/getallmail"
#}}}
#{{{ Enable aliases to be sudo’ed
alias sudo='sudo '
#}}}
#{{{ quick jump to files and directories
function vimrc() {
    $EDITOR -c ':e $MYVIMRC'
}
#}}}
#{{{ always recursive and verbose
alias cp="cp -rv"
alias rm="rm -rv"
alias mv='mv -v'
#}}}
#{{{ web dev stuff
alias speedtest="wget -O /dev/null \
 http://speedtest.wdc01.softlayer.com/downloads/test10.zip"
alias wget="wget -c --no-check-certificate"
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
alias l="ls -Fl ${colorflag}"
alias ls-a="ls -Fa ${colorflag}"
# List all files colorized in long format, including dot files
alias la="ls -Fla ${colorflag}"
alias ll="ls -Fl ${colorflag}"
# List only directories
alias lsd='ls -Fl ${colorflag} | grep "^d"'
# Always use color output for `ls`
alias ls="command ls -F ${colorflag}"
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
alias rip="dig +short myip.opendns.com @resolver1.opendns.com"
alias ip="ip --color=auto"
#}}}
# {{{ utilities
# Canonical hex dump; some systems have this symlinked
command -v hd >/dev/null || alias hd="hexdump -C"
# URL-encode strings
alias urlencode='python2 -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1]);"'
# Ring the terminal bell, and put a badge on Terminal.app’s Dock icon
# (useful when executing time-consuming commands)
alias bell="cvlc --play-and-exit $HOME/.config/sounds/beep.mp3 2> /dev/null; tput bel"
alias less="less -N"
# One of @janmoesen’s ProTip™s
for method in GET HEAD POST PUT DELETE TRACE OPTIONS; do
    alias "$method"="lwp-request -m '$method'"
done
alias starwars="telnet towel.blinkenlights.nl"
alias hangups="$HOME/Sites/hangups/bin/hangups"
#}}}
#{{{ tmux
alias tmux="tmux -f $XDG_CONFIG_HOME/tmux.d/tmux.conf"
alias tkill="s tmux; tmux kill-session -t"
alias tkills="tmux kill-server"
alias tns="tmux new-session"
alias tls="tmux ls"
#}}}
#{{{ uncategorized aliases
alias poweroff="systemctl poweroff"
alias reboot="systemctl reboot"
alias mute="amixer -c 0 -- set Master playback -1000dB > /dev/null"
alias unmute="amixer -c 0 -- set Master playback -20dB > /dev/null"
#}}}
#{{{ utility commands
alias q="exit"
alias mypw="pwgen -c -n -s -y 16 -1"
alias ndate="date \"+%d-%m-%y\""
# easy reload of zsh stuff
alias rl="exec zsh -l"
alias zconfig="$EDITOR $ZDOTDIR/zshrc"
#}}}
#{{{ git configs
alias g="git"
alias gca="git commit --all --verbose"
alias gco="git checkout"
alias gs="git status --short"
#}}}
# {{{ Global aliases
alias -g bg="& disown"
# }}}
# {{{ docker
alias dc='docker-compose'
alias drp='docker-compose exec php-fpm'
alias drc='docker-compose exec'
alias dupd='docker-compose up -d'
alias dupbd='docker-compose up -d --build'
alias dud='docker-compose down'
#}}}

#{{{ profiling tools
PROFILE_STARTUP=false
if [[ "$PROFILE_STARTUP" == true ]]; then
    zmodload zsh/zprof
    # http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html
    PS4=$'%D{%M%S%.} %N:%i> '
    exec 3>&2 2>$HOME/startlog.$$
    setopt xtrace prompt_subst
fi
#}}}
#{{{ install functions
MODULES_DIR="$HOME/.zsh_modules"
UPDATE_INTERVAL=5
function _net_test() {
    netflag="/tmp/network_down"
    touch $netflag
    if [[ $(curl -s --max-time 2 -I http://example.com | sed 's/^[^ ]*  *\([0-9]\).*/\1/; 1q')  -eq 2 ]]; then
        command rm $netflag >/dev/null
        return 0
    elif [[ -a $netflag ]]; then
        return 1
    elif [[ ! $(curl -s --max-time 2 -I http://example.com | sed 's/^[^ ]*  *\([0-9]\).*/\1/; 1q')  -eq 2 ]]; then
        echo "NO NETWORK"
        tput bel
        touch $netflag
        return 1
    else
        command rm $netflag >/dev/null
        return 0
    fi
}
function _update() {
    if [[ -z $UPDATE_INTERVAL ]];
    then
        UPDATE_INTERVAL=30
    fi

    if [[ ! -a $MODULES_DIR/$1/.updatetime ]];
    then
        echo 0 > "$MODULES_DIR/$1/.updatetime"
    fi

    day=$((24 * 60 * 60 ))
    gap=$(( $UPDATE_INTERVAL * $day ))
    diff="$(( $(date +'%s') - $(cat $MODULES_DIR/$1/.updatetime) ))"

    if [[ $diff -gt $gap ]]; then
        (
        _net_test
        if [[ $? -eq 1 ]]; then
            return
        fi
        echo  "$2"
        builtin cd "$MODULES_DIR/$1/" && git pull --rebase
        echo "\n"
        )
        date +'%s' > "$MODULES_DIR/$1/.updatetime"
    fi
}

function source_or_install() {

    if [[ $2 =~ '^(git@|https)' ]]; then
        tillcolon=("${(@s/:/)2}")
        cloneurl=$2
        if [[ $tillcolon[1] = 'https' ]]; then
            atslashes=("${(@s#/#)tillcolon}")
            location="$atslashes[-2]/$atslashes[-1]"
        else
            location="$tillcolon[-1]"
        fi
    else
        cloneurl="git@github.com:$2"
        location=$2
    fi
    if [[ -a $1 ]] then;
        source $1
    else
        _net_test
        if [[ $? -eq 1 ]]; then
            echo "oops"
            return
        fi

        git clone --depth 3 "$cloneurl" "$MODULES_DIR/$location"
        find $MODULES_DIR -type d -delete 2>/dev/null
        if [[ ! -d $MODULES_DIR/$location  ]]; then
            echo "nothing cloned; trying https"
        fi
        date +'%s' > "$MODULES_DIR/$location/.updatetime"
        echo "\n"
        source $HOME/.zshrc
    fi
    _update $location
}

function force_updates() {
    (
    builtin cd $MODULES_DIR
    find ./ -type f -name '.updatetime' -delete
    source $HOME/.zshrc
    )
}
#}}}
#{{{ The base package, containing all the essentials, including my prompt
source ~/.zprofile
source_or_install "$MODULES_DIR/yramagicman/zsh-aliases/init.zsh" "git@gitlab.com:yramagicman/zsh-aliases"
#}}}
# {{{ ls colors, and other colors
# export LS_COLORS='no=00:fi=00:di=01;34:ln=00;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=41;33;01:ex=00;32:*.cmd=00;32:*.exe=01;32:*.com=01;32:*.bat=01;32:*.btm=01;32:*.dll=01;32:*.tar=00;31:*.tbz=00;31:*.tgz=00;31:*.rpm=00;31:*.deb=00;31:*.arj=00;31:*.taz=00;31:*.lzh=00;31:*.lzma=00;31:*.zip=00;31:*.zoo=00;31:*.z=00;31:*.Z=00;31:*.gz=00;31:*.bz2=00;31:*.tb2=00;31:*.tz2=00;31:*.tbz2=00;31:*.xz=00;31:*.avi=01;35:*.bmp=01;35:*.dl=01;35:*.fli=01;35:*.gif=01;35:*.gl=01;35:*.jpg=01;35:*.jpeg=01;35:*.mkv=01;35:*.mng=01;35:*.mov=01;35:*.mp4=01;35:*.mpg=01;35:*.pcx=01;35:*.pbm=01;35:*.pgm=01;35:*.png=01;35:*.ppm=01;35:*.svg=01;35:*.tga=01;35:*.tif=01;35:*.webm=01;35:*.webp=01;35:*.wmv=01;35:*.xbm=01;35:*.xcf=01;35:*.xpm=01;35:*.aiff=00;32:*.ape=00;32:*.au=00;32:*.flac=00;32:*.m4a=00;32:*.mid=00;32:*.mp3=00;32:*.mpc=00;32:*.ogg=00;32:*.voc=00;32:*.wav=00;32:*.wma=00;32:*.wv=00;32:'
# }}}
#{{{ Set zsh options for general runtime.
#
# Load the prompt system and completion system and initilize them
autoload -Uz compinit promptinit
compinit
promptinit

# load colors
autoload -U colors && colors

# Use case-insensitve globbing.
unsetopt CASE_GLOB
# glob dotfiles as well
setopt GLOBDOTS

# Automatically change directory if a directory is entered
setopt autocd
setopt extendedglob

#
# Smart URLs
#

autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

#
# General
#

# Allow brace character class list expansion.
setopt BRACE_CCL
# Combine zero-length punctuation characters (accents) with the base character.
setopt COMBINING_CHARS
 # Allow 'Henry''s Garage' instead of 'Henry'\''s Garage'.
setopt RC_QUOTES
# Don't print a warning message if a mail file has been accessed.
unsetopt MAIL_WARNING

#
# Jobs
#
# List jobs in the long format by default.
setopt LONG_LIST_JOBS
# Attempt to resume existing job before creating a new process.
setopt AUTO_RESUME
# Report status of background jobs immediately.
setopt NOTIFY
# Don't run all background jobs at a lower priority.
unsetopt BG_NICE
# Don't kill jobs on shell exit.
unsetopt HUP
# Don't report on jobs when shell exit.
unsetopt CHECK_JOBS

# turn on corrections
setopt correct

# use emacs bindings
bindkey -e
# Disable some shell keyboard shortcuts
stty -ixon > /dev/null 2>/dev/null

#}}}
#{{{ completion
#{{{ options
# Complete from both ends of a word.
setopt COMPLETE_IN_WORD
# Move cursor to the end of a completed word.
setopt ALWAYS_TO_END
# Perform path search even on command names with slashes.
setopt PATH_DIRS
# Show completion menu on a successive tab press.
setopt AUTO_MENU
# Automatically list choices on ambiguous completion.
setopt AUTO_LIST
# If completed parameter is a directory, add a trailing slash.
setopt AUTO_PARAM_SLASH
setopt NO_COMPLETE_ALIASES
# Do not autoselect the first completion entry.
unsetopt MENU_COMPLETE
# Disable start/stop characters in shell editor.
unsetopt FLOW_CONTROL
#}}}
#{{{ zstyle
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:corrections' format ' %F{green}-- %d (errors: %e) --%f'
zstyle ':completion:*:descriptions' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "$HOME/.zcompcache"
zstyle ':completion:*' list-colors $LS_COLORS
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' rehash true
#}}}
#}}}
#{{{ history
HISTFILE="$HOME/.zhistory"
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory notify
unsetopt beep extendedglob nomatch
# Treat the '!' character specially during expansion.
setopt BANG_HIST
# Write to the history file immediately, not when the shell exits.
setopt INC_APPEND_HISTORY
# Share history between all sessions.
setopt SHARE_HISTORY
# Expire a duplicate event first when trimming history.
setopt HIST_EXPIRE_DUPS_FIRST
# Do not record an event that was just recorded again.
setopt HIST_IGNORE_DUPS
# Delete an old recorded event if a new event is a duplicate.
setopt HIST_IGNORE_ALL_DUPS
# Do not display a previously found event.
setopt HIST_FIND_NO_DUPS
# Do not record an event starting with a space.
setopt HIST_IGNORE_SPACE
# Do not write a duplicate event to the history file.
setopt HIST_SAVE_NO_DUPS
# Do not execute immediately upon history expansion.
setopt HIST_VERIFY
# Show timestamp in history
setopt EXTENDED_HISTORY
#}}}
#{{{ prompt. If serenity isn't there use a default
if [[ $(  prompt -l | grep serenity  ) ]]; then
    prompt serenity
else
    prompt adam2
fi
#}}}
#{{{ grab the rest of the packages
source_or_install "$MODULES_DIR/zsh-users/zsh-completions/zsh-completions.plugin.zsh" zsh-users/zsh-completions
source_or_install "$MODULES_DIR/marzocchi/zsh-notify/notify.plugin.zsh" marzocchi/zsh-notify
source_or_install "$MODULES_DIR/srijanshetty/zsh-pandoc-completion/zsh-pandoc-completion.plugin.zsh" srijanshetty/zsh-pandoc-completion
# source_or_install "$MODULES_DIR/Tarrasch/zsh-autoenv/autoenv.plugin.zsh" Tarrasch/zsh-autoenv
if [[ -f /usr/share/doc/pkgfile/command-not-found.zsh ]]; then
    source /usr/share/doc/pkgfile/command-not-found.zsh
fi
if [[ -d /usr/share/fzf ]]; then
    source /usr/share/fzf/key-bindings.zsh
    source /usr/share/fzf/completion.zsh
fi

#}}}
#{{{ lazy load stuff
if [[ "$TMUX" != '' ]]; then
    if [[ -z "$(pgrep tmuxcopy )" ]];
    then
        tmuxcopy &
    fi
fi

function workon() {
    echo " working.."
    if [[ -f $VIRTUALENVWRAPPER_SCRIPT ]] then
        source $VIRTUALENVWRAPPER_SCRIPT   &> /dev/null
        workon "$@"
    else
        echo "virtualenvwrapper.sh not at  /usr/bin/virtualenvwrapper.sh"
    fi
}

function z() {
    source_or_install "$MODULES_DIR/rupa/z/z.sh" rupa/z
    _z $@
}

function artisan() {
    source_or_install "$MODULES_DIR/crazybooot/laravel-zsh-plugin/laravel-artisan.plugin.zsh" crazybooot/laravel-zsh-plugin
    php artisan $*
}
setopt NO_BEEP
#}}}
#{{{ key bindings,
# Allow command line editing in an external editor.
autoload -Uz edit-command-line
zle -N edit-command-line

bindkey -M emacs "\C-X\C-E" edit-command-line
#}}}
#{{{ virtualenv stuff
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Sites
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
export VIRTUALENVWRAPPER_SCRIPT=/usr/bin/virtualenvwrapper.sh
#}}}
#{{{ random user opions
export XDG_CONFIG_HOME=$HOME/.config
export BROWSER=firefox
export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
export FZF_CTRL_T_COMMAND='ag --hidden --ignore .git -g ""'
#}}}
#{{{cdr, persistent cd
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
DIRSTACKFILE="$HOME/.cache/zsh/dirs"

# make DIRSTACKFILE if it's not there
if [[ ! -a $DIRSTACKFILE ]]; then
    mkdir -p $DIRSTACKFILE[0,-5]
    touch $DIRSTACKFILE
fi

if [[ -f $DIRSTACKFILE ]] && [[ $#dirstack -eq 0 ]]; then
    dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
fi

chpwd() {
    print -l $PWD ${(u)dirstack} >>$DIRSTACKFILE
    d="$(sort -u $DIRSTACKFILE )"
    echo "$d" > $DIRSTACKFILE

}

DIRSTACKSIZE=20

setopt AUTO_PUSHD PUSHD_SILENT PUSHD_TO_HOME

# Remove duplicate entries
setopt PUSHD_IGNORE_DUPS

# This reverts the +/- operators.
setopt PUSHD_MINUS

#}}}
#{{{ start tmux,
# if [[ -z "$TMUX" && -z "$EMACS" && -z "$VIM" && -z "$SSH_TTY" ]]; then
#     if [[ -z $( pgrep tmux$ ) ]] then
#         # s tmux
#     fi
# fi
# }}}
#{{{ end profiling script
if [[ "$PROFILE_STARTUP" == true ]]; then
    unsetopt xtrace
    exec 2>&3 3>&-
    zprof > ~/zshprofile
fi
#}}}

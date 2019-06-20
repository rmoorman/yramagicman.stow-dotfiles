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
export XDG_CONFIG_HOME=$HOME/.config

if [[ -z  $XDG_CONFIG_HOME ]];then
    MODULES_DIR="$HOME/.zsh_modules"
else
    MODULES_DIR="$XDG_CONFIG_HOME/zsh_plugins"
fi

fpath=("$HOME/.zsh" $fpath)
autoload $HOME/.zsh/*
#}}}
#{{{ Load packages
source ~/.zprofile
function pkgs() {
    local PKGS=(
        "$MODULES_DIR/yramagicman/zsh-aliases/init.zsh" "git@gitlab.com:yramagicman/zsh-aliases"
        "$MODULES_DIR/agkozak/zsh-z/zsh-z.plugin.zsh" agkozak/zsh-z
        "$MODULES_DIR/zsh-users/zsh-completions/zsh-completions.plugin.zsh" zsh-users/zsh-completions
    )
    load_pkgs $PKGS
    z_update $PKGS
}
pkgs
unfunction pkgs >/dev/null 2>&1
#}}}
# {{{ ls colors, and other colors
# export LS_COLORS='no=00:fi=00:di=01;34:ln=00;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=41;33;01:ex=00;32:*.cmd=00;32:*.exe=01;32:*.com=01;32:*.bat=01;32:*.btm=01;32:*.dll=01;32:*.tar=00;31:*.tbz=00;31:*.tgz=00;31:*.rpm=00;31:*.deb=00;31:*.arj=00;31:*.taz=00;31:*.lzh=00;31:*.lzma=00;31:*.zip=00;31:*.zoo=00;31:*.z=00;31:*.Z=00;31:*.gz=00;31:*.bz2=00;31:*.tb2=00;31:*.tz2=00;31:*.tbz2=00;31:*.xz=00;31:*.avi=01;35:*.bmp=01;35:*.dl=01;35:*.fli=01;35:*.gif=01;35:*.gl=01;35:*.jpg=01;35:*.jpeg=01;35:*.mkv=01;35:*.mng=01;35:*.mov=01;35:*.mp4=01;35:*.mpg=01;35:*.pcx=01;35:*.pbm=01;35:*.pgm=01;35:*.png=01;35:*.ppm=01;35:*.svg=01;35:*.tga=01;35:*.tif=01;35:*.webm=01;35:*.webp=01;35:*.wmv=01;35:*.xbm=01;35:*.xcf=01;35:*.xpm=01;35:*.aiff=00;32:*.ape=00;32:*.au=00;32:*.flac=00;32:*.m4a=00;32:*.mid=00;32:*.mp3=00;32:*.mpc=00;32:*.ogg=00;32:*.voc=00;32:*.wav=00;32:*.wma=00;32:*.wv=00;32:'
# }}}
#{{{ Set zsh options for general runtime.
#
# Load the prompt system and completion system and initilize them
autoload -Uz compinit promptinit
# Load and initialize the completion system ignoring insecure directories with a
# cache time of 20 hours, so it should almost always regenerate the first time a
# shell is opened each day.
autoload -Uz compinit
_comp_files=(${ZDOTDIR:-$HOME}/.zcompdump(Nm-20))
if (( $#_comp_files )); then
  compinit -i -C
else
  compinit -i
fi
unset _comp_files
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
setopt MENU_COMPLETE
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
zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'
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

        setopt nonotify nomonitor
        tmuxcopy &
        disown
        unsetopt nonotify nomonitor
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
bindkey -M emacs "^[[Z" reverse-menu-complete
bindkey -M emacs "\C-X\C-E" edit-command-line

# Runs bindkey but for all of the keymaps. Running it with no arguments will
# print out the mappings for all of the keymaps.
function bindkey-all {
    local keymap=''
    for keymap in $(bindkey -l); do
        [[ "$#" -eq 0 ]] && printf "#### %s\n" "${keymap}" 1>&2
        bindkey -M "${keymap}" "$@"
    done
}
#}}}
#{{{ virtualenv stuff
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Sites
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
export VIRTUALENVWRAPPER_SCRIPT=/usr/bin/virtualenvwrapper.sh
#}}}
#{{{ random user opions
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
if [[ -z "$TMUX" && -z "$EMACS" && -z "$VIM" && -z "$SSH_TTY" ]]; then
    if [[ -z $( pgrep tmux$ ) ]] then
        s tmux
    fi
fi
# }}}
#{{{ end profiling script
if [[ "$PROFILE_STARTUP" == true ]]; then
    unsetopt xtrace
    exec 2>&3 3>&-
    zprof > ~/zshprofile$(date +'%s')
fi
#}}}

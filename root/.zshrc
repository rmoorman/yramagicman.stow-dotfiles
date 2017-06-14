#{{{ Set defaults
CONFIG_DIR="$HOME/Gits"
autoload -Uz compinit promptinit
compinit
promptinit
unsetopt CASE_GLOB
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

 #}}}
# {{{ colors
if [[ $TERM = *256color* || $TERM = *rxvt* ]]; then
  _prompt_lime_colors=(
    "%F{69}"  # Turquoise
    "%F{166}" # Orange
    "%F{135}" # Purple
    "%F{161}" # Hotpink
    "%F{40}" # Limegreen
    "%F{245}" # dark grey
  )
else
  _prompt_lime_colors=(
    "%F{cyan}"
    "%F{yellow}"
    "%F{magenta}"
    "%F{red}"
    "%F{green}"
    "%F{grey}"
  )
fi
#}}}
# {{{ VCS in prompt options
# Formats:
#   %b - branchname
#   %u - unstagedstr (see below)
#   %c - stagedstr (see below)
#   %a - action (e.g. rebase-i)
#   %R - repository path
#   %S - path in the repository
local branch_format="(${_prompt_lime_colors[1]}%b%f%u%c)"
local action_format="(${_prompt_lime_colors[5]}%a%f)"
local unstaged_format="${_prompt_lime_colors[2]}*%f"
local staged_format="${_prompt_lime_colors[5]}+%f"

# Set vcs_info parameters.
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*:prompt:*' check-for-changes true
zstyle ':vcs_info:*:prompt:*' unstagedstr "${unstaged_format}"
zstyle ':vcs_info:*:prompt:*' stagedstr "${staged_format}"
zstyle ':vcs_info:*:prompt:*' actionformats "${branch_format}${action_format}"
zstyle ':vcs_info:*:prompt:*' formats "${branch_format}"
zstyle ':vcs_info:*:prompt:*' nvcsformats   ""
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
#}}}
#{{{ Prompt

precmd() {
    if git rev-parse --git-dir > /dev/null 2>&1; then
        str=$( git rev-parse --show-toplevel)
        split=("${(@s#/#)str}")
        git_root=$split[-1]
        unset str
    else
        git_root=''
    fi
    branch_format=" (${_prompt_lime_colors[1]}%b%f%u%c ${_prompt_lime_colors[1]}${git_root}%f)"

    zstyle ':vcs_info:*:prompt:*' formats "${branch_format}"


    vcs_info 'prompt'

}
setopt prompt_subst
PROMPT='
 %B${_prompt_lime_colors[5]}%m%f %~${vcs_info_msg_0_}
 ->%b '
RPROMPT='${_prompt_lime_colors[6]}[ %D{%I:%M %P} ]%f'
#}}}
#{{{ source and load aliases and plugins

function source_or_clone() {

if [[ -a $1 ]]; then
    source $1
else
    echo "$1 not found "
    read -k 1 -r \
        "REPLY?Do you want to clone it into $CONFIG_DIR "
    if [[ $REPLY =~ ^[yY]$ ]]; then
        mkdir -p $CONFIG_DIR
        builtin cd  $CONFIG_DIR
        echo "\n"
        git clone $2
        source $1
        builtin cd
    fi
fi
}
source $HOME/.zprofile

source_or_clone $CONFIG_DIR/z/z.sh 'https://github.com/rupa/z.git'
source_or_clone $CONFIG_DIR/zsh-completions/zsh-completions.plugin.zsh 'git://github.com/zsh-users/zsh-completions.git'
source_or_clone $CONFIG_DIR/zsh-aliases/init.zsh 'git@github.com:yramagicman/zsh-aliases.git'

#}}}
#{{{ completion
#{{{ options
setopt COMPLETE_IN_WORD    # Complete from both ends of a word.
setopt ALWAYS_TO_END       # Move cursor to the end of a completed word.
setopt PATH_DIRS           # Perform path search even on command names with slashes.
setopt AUTO_MENU           # Show completion menu on a successive tab press.
setopt AUTO_LIST           # Automatically list choices on ambiguous completion.
setopt AUTO_PARAM_SLASH    # If completed parameter is a directory, add a trailing slash.
setopt COMPLETE_ALIASES
unsetopt MENU_COMPLETE     # Do not autoselect the first completion entry.
unsetopt FLOW_CONTROL      # Disable start/stop characters in shell editor.
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
zstyle ':completion:*' list-colors 'rs=0' 'di=01;34' 'ln=01;36' 'mh=00' 'pi=40;33' 'so=01;35' 'do=01;35' 'bd=40;33;01' 'cd=40;33;01' 'or=40;31;01' 'mi=00' 'su=37;41' 'sg=30;43' 'ca=30;41' 'tw=30;42' 'ow=34;42' 'st=37;44' 'ex=01;32' '*.tar=01;31' '*.tgz=01;31' '*.arc=01;31' '*.arj=01;31' '*.taz=01;31' '*.lha=01;31' '*.lz4=01;31' '*.lzh=01;31' '*.lzma=01;31' '*.tlz=01;31' '*.txz=01;31' '*.tzo=01;31' '*.t7z=01;31' '*.zip=01;31' '*.z=01;31' '*.Z=01;31' '*.dz=01;31' '*.gz=01;31' '*.lrz=01;31' '*.lz=01;31' '*.lzo=01;31' '*.xz=01;31' '*.zst=01;31' '*.tzst=01;31' '*.bz2=01;31' '*.bz=01;31' '*.tbz=01;31' '*.tbz2=01;31' '*.tz=01;31' '*.deb=01;31' '*.rpm=01;31' '*.jar=01;31' '*.war=01;31' '*.ear=01;31' '*.sar=01;31' '*.rar=01;31' '*.alz=01;31' '*.ace=01;31' '*.zoo=01;31' '*.cpio=01;31' '*.7z=01;31' '*.rz=01;31' '*.cab=01;31' '*.jpg=01;35' '*.jpeg=01;35' '*.mjpg=01;35' '*.mjpeg=01;35' '*.gif=01;35' '*.bmp=01;35' '*.pbm=01;35' '*.pgm=01;35' '*.ppm=01;35' '*.tga=01;35' '*.xbm=01;35' '*.xpm=01;35' '*.tif=01;35' '*.tiff=01;35' '*.png=01;35' '*.svg=01;35' '*.svgz=01;35' '*.mng=01;35' '*.pcx=01;35' '*.mov=01;35' '*.mpg=01;35' '*.mpeg=01;35' '*.m2v=01;35' '*.mkv=01;35' '*.webm=01;35' '*.ogm=01;35' '*.mp4=01;35' '*.m4v=01;35' '*.mp4v=01;35' '*.vob=01;35' '*.qt=01;35' '*.nuv=01;35' '*.wmv=01;35' '*.asf=01;35' '*.rm=01;35' '*.rmvb=01;35' '*.flc=01;35' '*.avi=01;35' '*.fli=01;35' '*.flv=01;35' '*.gl=01;35' '*.dl=01;35' '*.xcf=01;35' '*.xwd=01;35' '*.yuv=01;35' '*.cgm=01;35' '*.emf=01;35' '*.ogv=01;35' '*.ogx=01;35' '*.aac=00;36' '*.au=00;36' '*.flac=00;36' '*.m4a=00;36' '*.mid=00;36' '*.midi=00;36' '*.mka=00;36' '*.mp3=00;36' '*.mpc=00;36' '*.ogg=00;36' '*.ra=00;36' '*.wav=00;36' '*.oga'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' rehash true
#}}}
#}}}
#{{{ History options

# The path to the history file.
HISTFILE="$HOME/.zhistory"
# The maximum number of events to save in the internal history.
HISTSIZE=10000
# The maximum number of events to save in the history file.
SAVEHIST=10000

#
# Options
#

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

#}}}
# {{{ lazy load stuff
if [[ "$TMUX" != '' ]]; then

    if [[ -z "$(pgrep tmuxcopy )" ]];
    then
        tmuxcopy &
    fi

fi
function workon() {
        source /usr/bin/virtualenvwrapper.sh  &> /dev/null
        workon "$@"
}
setopt NO_BEEP
xset b off
#}}}
#{{{ key bindings,
# define function that retrieves and runs last command
function run-again {
    # get previous history item
    zle up-history
    # confirm command
    zle accept-line
}

# define run-again widget from function of the same name
zle -N run-again

# bind widget to Ctrl+X in viins mode
bindkey -e '^y' run-again

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
export PATH=$PATH:$HOME/bin/
export BROWSER=chromium
#}}}
# {{{cdr, persistent cd
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

}

DIRSTACKSIZE=20

setopt AUTO_PUSHD PUSHD_SILENT PUSHD_TO_HOME

# Remove duplicate entries
setopt PUSHD_IGNORE_DUPS

# This reverts the +/- operators.
setopt PUSHD_MINUS
# }}}
#{{{ start tmux,
if [[ -z "$TMUX" && -z "$EMACS" && -z "$VIM" && -z "$SSH_TTY" ]]; then
    s tmux
fi
#}}}

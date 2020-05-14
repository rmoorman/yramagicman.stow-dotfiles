#!usr/bin/env zsh
#{{{ start xserver when necessary
if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
    exec startx "$HOME/.config/X11/xinitrc"
fi
#}}}
#{{{ set config variables and ensure files are linked
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
#}}}
#{{{ env vars
# export WORKON_HOME=$HOME/.virtualenvs
# export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
# export VIRTUALENVWRAPPER_SCRIPT=/usr/bin/virtualenvwrapper.sh
# export PROJECT_HOME=$HOME/Sites
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export UPDATE_INTERVAL=15
export MODULES_DIR="$ZDOTDIR/plugins"
export ZSHZ_DATA="$XDG_CACHE_HOME/zsh/z"
export CHROME_BIN=/usr/bin/chromium
export GIT_TEMPLATE_DIR="$XDG_CONFIG_HOME/git/template"
export XINITRC="$XDG_CONFIG_HOME/X11/xinitrc"
export XSERVERRC="$XDG_CONFIG_HOME/X11/xserverrc"
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0"
export LESSHISTFILE="/dev/null"
export WGETRC="$XDG_CONFIG_HOME/wget/wgetrc"
export INPUTRC="$XDG_CONFIG_HOME/inputrc"
export MU_HOME="$XDG_CACHE_HOME/mu"
export PLTUSERHOME="$XDG_DATA_HOME/racket"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/pass"


if ( hash fzf > /dev/null 2>&1 ); then
    export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
    export FZF_CTRL_T_COMMAND='ag --hidden --ignore .git -g ""'
fi
#}}}
#{{{ Browser
[[ "$OSTYPE" == darwin* ]] && export BROWSER='open' || export BROWSER=firefox
export WWW_HOME="$XDG_CONFIG_HOME/www_data"
#}}}
#{{{ Editors
export EDITOR='emacsclient -c'
export VISUAL='vim'
export PAGER='less'
#}}}
#{{{ Language
[[ -z "$LANG" ]] && export LANG='en_US.UTF-8'
#}}}
#{{{ Paths

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

# Set the the list of directories that cd searches.
cdpath=(
  "$HOME/Documents/"
  "$HOME/Documents/dots"
)

# Set the list of directories that Zsh searches for programs.
path=(
    $HOME/bin
    $HOME/.local/bin
    $HOME/.gem/ruby/2.6.0/bin
    $HOME/.config/composer/vendor/bin
    /usr/local/{bin,sbin}
    $path
)

#}}}
#{{{ Less

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Set the Less input preprocessor.
# Try both `lesspipe` and `lesspipe.sh` as either might exist on a system.
if (( $#commands[(i)lesspipe(|.sh)] )); then
    export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
fi

#}}}

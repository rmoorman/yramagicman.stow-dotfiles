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
#{{{ ls colors, and other colors
# export LS_COLORS='no=00:fi=00:di=01;34:ln=00;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=41;33;01:ex=00;32:*.cmd=00;32:*.exe=01;32:*.com=01;32:*.bat=01;32:*.btm=01;32:*.dll=01;32:*.tar=00;31:*.tbz=00;31:*.tgz=00;31:*.rpm=00;31:*.deb=00;31:*.arj=00;31:*.taz=00;31:*.lzh=00;31:*.lzma=00;31:*.zip=00;31:*.zoo=00;31:*.z=00;31:*.Z=00;31:*.gz=00;31:*.bz2=00;31:*.tb2=00;31:*.tz2=00;31:*.tbz2=00;31:*.xz=00;31:*.avi=01;35:*.bmp=01;35:*.dl=01;35:*.fli=01;35:*.gif=01;35:*.gl=01;35:*.jpg=01;35:*.jpeg=01;35:*.mkv=01;35:*.mng=01;35:*.mov=01;35:*.mp4=01;35:*.mpg=01;35:*.pcx=01;35:*.pbm=01;35:*.pgm=01;35:*.png=01;35:*.ppm=01;35:*.svg=01;35:*.tga=01;35:*.tif=01;35:*.webm=01;35:*.webp=01;35:*.wmv=01;35:*.xbm=01;35:*.xcf=01;35:*.xpm=01;35:*.aiff=00;32:*.ape=00;32:*.au=00;32:*.flac=00;32:*.m4a=00;32:*.mid=00;32:*.mp3=00;32:*.mpc=00;32:*.ogg=00;32:*.voc=00;32:*.wav=00;32:*.wma=00;32:*.wv=00;32:'
# }}}
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

# Other program settings:
if [[ -d '/usr/local/opt/php@7.3' ]]; then
    export PATH="/usr/local/opt/php@7.3/bin:$PATH"
    export PATH="/usr/local/opt/php@7.3/sbin:$PATH"
fi

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
export EDITOR='vim'
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
# cdpath=(
#   $cdpath
# )

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

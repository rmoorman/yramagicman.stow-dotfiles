local dotdir="$HOME/.config/zsh"
if [[ ! -f "$dotdir/.zshrc" ]] && [[ -f "$dotdir/zshrc" ]]
then
    (
    cd "$dotdir" || exit;
    for f in *; do
        ln -s "$PWD/$f" "$PWD/.$f"
    done
    rm -rf .pkg .zsh
)
elif [[ -f "$dotdir/.zshrc" ]]
then
    ZDOTDIR="$dotdir"
else
    if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
        echo 'no zshrc' > $HOME/nozshrc
        exec startx
    fi
fi
#{{{ ls colors, and other colors
# export LS_COLORS='no=00:fi=00:di=01;34:ln=00;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=41;33;01:ex=00;32:*.cmd=00;32:*.exe=01;32:*.com=01;32:*.bat=01;32:*.btm=01;32:*.dll=01;32:*.tar=00;31:*.tbz=00;31:*.tgz=00;31:*.rpm=00;31:*.deb=00;31:*.arj=00;31:*.taz=00;31:*.lzh=00;31:*.lzma=00;31:*.zip=00;31:*.zoo=00;31:*.z=00;31:*.Z=00;31:*.gz=00;31:*.bz2=00;31:*.tb2=00;31:*.tz2=00;31:*.tbz2=00;31:*.xz=00;31:*.avi=01;35:*.bmp=01;35:*.dl=01;35:*.fli=01;35:*.gif=01;35:*.gl=01;35:*.jpg=01;35:*.jpeg=01;35:*.mkv=01;35:*.mng=01;35:*.mov=01;35:*.mp4=01;35:*.mpg=01;35:*.pcx=01;35:*.pbm=01;35:*.pgm=01;35:*.png=01;35:*.ppm=01;35:*.svg=01;35:*.tga=01;35:*.tif=01;35:*.webm=01;35:*.webp=01;35:*.wmv=01;35:*.xbm=01;35:*.xcf=01;35:*.xpm=01;35:*.aiff=00;32:*.ape=00;32:*.au=00;32:*.flac=00;32:*.m4a=00;32:*.mid=00;32:*.mp3=00;32:*.mpc=00;32:*.ogg=00;32:*.voc=00;32:*.wav=00;32:*.wma=00;32:*.wv=00;32:'
# }}}
#{{{ env vars
export XDG_CONFIG_HOME=$HOME/.config
export UPDATE_INTERVAL=15
export MODULES_DIR="$ZDOTDIR/plugins"
export ZSHZ_DATA="$HOME/.cache/zsh/z"
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Sites
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
export VIRTUALENVWRAPPER_SCRIPT=/usr/bin/virtualenvwrapper.sh
export CHROME_BIN=/usr/bin/chromium

if [[ -d '/usr/local/opt/php@7.3' ]]; then
    export PATH="/usr/local/opt/php@7.3/bin:$PATH"
    export PATH="/usr/local/opt/php@7.3/sbin:$PATH"
fi
export BROWSER=firefox
export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
export FZF_CTRL_T_COMMAND='ag --hidden --ignore .git -g ""'
#}}}

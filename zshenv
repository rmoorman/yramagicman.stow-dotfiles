local dotdir="$HOME/.config/zsh"
if [[ ! -f "$dotdir/.zshrc" ]] && [[ -f "$dotdir/zshrc" ]]
then
    (
    cd "$dotdir" || exit;
    for f in *; do
        ln -s "$PWD/$f" "$PWD/.$f"
    done
    rm -rf .plugins
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

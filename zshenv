local dotdir="$HOME/.config/zsh"
if [[ ! -f "$dotdir/.zshrc" ]] && [[ -f "$dotdir/zshrc" ]]
    then
    ( cd "$dotdir" || exit;
    for f in *; do
        ln -s "$PWD/$f" "$PWD/.$f"
    done
    rm -rf .plugins
    )
fi
ZDOTDIR="$dotdir"

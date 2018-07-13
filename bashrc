source "$HOME/.bash_prompt"
# auto ls on cd
function cd() {
    builtin cd "$@" && ls
}

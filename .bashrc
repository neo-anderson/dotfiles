# The below line starts an emacs daemon if it doesn't exist
# https://stackoverflow.com/questions/5570451/how-to-start-emacs-server-only-if-it-is-not-started
export ALTERNATE_EDITOR=""
alias e='emacsclient -nw' # CLI
alias ew='emacsclient -c' # GUI. Create frame

alias avas='aws-vault exec audioscience_poweruser_assumerole --prompt=terminal -- '
alias avas1='aws-vault exec audioscience_poweruser_assumerole --duration=1h -- '
alias avas1env="aws-vault exec audioscience_poweruser_assumerole --duration=1h -- env | grep AWS | sed 's/\(.*\)=\(.*\)/export \1=\"\2\"/g' | pbcopy "

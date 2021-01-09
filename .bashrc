alias avas='aws-vault exec audioscience_poweruser_assumerole --prompt=terminal -- '
alias avas1='aws-vault exec audioscience_poweruser_assumerole --duration=1h -- '
alias avas1env="aws-vault exec audioscience_poweruser_assumerole --duration=1h -- env | grep AWS | sed 's/\(.*\)=\(.*\)/export \1=\"\2\"/g' | pbcopy "
alias e='emacsclient -nw '

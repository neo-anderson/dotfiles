# The below line starts an emacs daemon if it doesn't exist
# https://stackoverflow.com/questions/5570451/how-to-start-emacs-server-only-if-it-is-not-started
export ALTERNATE_EDITOR=""
alias e='emacsclient -nw' # CLI
alias ew='emacsclient -c' # GUI. Create frame

# Setting TERM so that colors are the same in and out of tmux sessions
export TERM=xterm-256color

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/aswin/miniconda/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/aswin/miniconda/etc/profile.d/conda.sh" ]; then
        . "/Users/aswin/miniconda/etc/profile.d/conda.sh"
    else
        export PATH="/Users/aswin/miniconda/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# Source work related config too
source ~/.bashrc_work

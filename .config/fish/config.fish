# config.fish may be run multiple times. https://github.com/fish-shell/fish-shell/issues/3803
# Only run user configuration block if status is interactive
if status is-interactive
  # >>> conda initialize >>>
  # !! Contents within this block are managed by 'conda init' !!
  eval /Users/aswin/miniconda/bin/conda "shell.fish" "hook" $argv | source
  # <<< conda initialize <<<

  # Keeping startup configuration in ~/.bashrc to share it with other shells
  bass source ~/.bashrc

  neofetch
else
  # echo shell is not interactive
  # ps -wfp %self
end


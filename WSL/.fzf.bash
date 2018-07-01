# Setup fzf
# ---------
if [[ ! "$PATH" == */home/rahul/.fzf/bin* ]]; then
  export PATH="$PATH:/home/rahul/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/rahul/.fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/home/rahul/.fzf/shell/key-bindings.bash"


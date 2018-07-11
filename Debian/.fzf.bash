# Setup fzf
# ---------
if [[ ! "$PATH" == */home/pi/.fzf/bin* ]]; then
  export PATH="$PATH:/home/pi/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/pi/.fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/home/pi/.fzf/shell/key-bindings.bash"


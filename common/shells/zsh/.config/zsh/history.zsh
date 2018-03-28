# Configure history file location and size
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000

# Avoid putting duplicates in history
# (this means can pressing up arrow wont go through 10 of same command)
setopt HIST_SAVE_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_FIND_NO_DUPS

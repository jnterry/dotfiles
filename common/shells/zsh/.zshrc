# Use emacs-like keybindings for editing commands
bindkey -e

# Ensure we include bin in the path
export PATH="$HOME/bin:$PATH"

ZSH_CONFIG_DIR=$HOME/.config/zsh/
for f in `ls ${ZSH_CONFIG_DIR} | grep \.sh$`
do
		source ${ZSH_CONFIG_DIR}$f
done

# Add z command to jump to recently used directories
source "$HOME/bin/z.sh"

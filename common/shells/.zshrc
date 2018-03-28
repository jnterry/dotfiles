# Use emacs-like keybindings for editing commands
bindkey -e

# Load common shell config
source "$HOME/.shells"

ZSH_CONFIG_DIR=$HOME/.config/zsh/
for f in `ls ${ZSH_CONFIG_DIR} | grep \.sh$`
do
		source ${ZSH_CONFIG_DIR}$f
done

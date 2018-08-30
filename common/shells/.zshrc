# Use emacs-like keybindings for editing commands
bindkey -e

# Load common shell config
for FILE in $HOME/.config/shells/*; do
		source $FILE
done

# Load zsh specifc config
ZSH_CONFIG_DIR=$HOME/.config/zsh/
for f in `ls ${ZSH_CONFIG_DIR} | grep \.sh$`
do
		source ${ZSH_CONFIG_DIR}$f
done

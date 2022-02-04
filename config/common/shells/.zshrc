# Fix for using emacs TRAMP to access this machine
# (Tramp uses dumb terminal, use single $ as prompt and then return from config)
[[ $TERM = "tramp" ]] && unsetopt zle && PS1='$ ' && return

# Use emacs-like keybindings for editing commands
bindkey -e

# Load common shell config
for FILE in $HOME/.config/shells/*.sh; do
		source $FILE
done

# Load zsh specifc config
ZSH_CONFIG_DIR=$HOME/.config/zsh/
for f in `ls ${ZSH_CONFIG_DIR} | grep \.sh$`
do
		source ${ZSH_CONFIG_DIR}$f
done

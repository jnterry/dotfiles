# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000

# Use emacs-like keybindings for editing commands
bindkey -e

ZSH_CONFIG_DIR=$HOME/.config/zsh/

for f in `ls ${ZSH_CONFIG_DIR} | grep \.sh$`
do
		source ${ZSH_CONFIG_DIR}$f
done

# Config for the zsh completion

# Ignore case in tab completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# Highlight the selected option, and use arrow keys to navigate the menu
zstyle ':completion:*' menu select

# Auto update tab-completion when something changes (EG: $PATH is altered)
zstyle ':completion:*' rehash true

# This script sets up config common to both bash and zsh shells

# Enable using programs in bin directory
export PATH="$HOME/bin:$PATH"

# Add z command to jump to recently used directories
source "$HOME/bin/z.sh"

# Add k for pretty directory listings
# https://github.com/supercrabtree/k
source "$HOME/bin/k.sh"

# added by travis gem
[ -f /home/jamie/.travis/travis.sh ] && source /home/jamie/.travis/travis.sh

if stat "$HOME/.nvm" > /dev/null 2> /dev/null ; then
		export NVM_DIR="$HOME/.nvm"
		[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
		[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
		export PATH="$PATH:$(nvm which node)"
fi

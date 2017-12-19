#!/usr/bin/env bash
#
# Installs all dotfiles to the appropriate locations

for d in `ls -d */`;
do
		printf "Installing '$d'... "
    ( stow $d --target $HOME )
		printf "Done\n"
done

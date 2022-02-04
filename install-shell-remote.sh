#!/usr/bin/env bash

SCRIPT=$(readlink -f "$0")
SCRIPTDIR=$(dirname $SCRIPT)

if [[ $# -ne 1 ]] ; then
		echo "Usage: $0 [REMOTE_HOST]"
		exit 1
fi

REMOTE=$1

echo "Install shell config to $REMOTE"

scp -pr $SCRIPTDIR/config/common/shells/bin $REMOTE:~/bin
scp -pr $SCRIPTDIR/config/common/shells/.zshrc $REMOTE:~/.zshrc
scp -pr $SCRIPTDIR/config/common/shells/.bashrc $REMOTE:~/.bashrc
scp -pr $SCRIPTDIR/config/common/shells/.config $REMOTE:~/.config

scp -pr $SCRIPTDIR/config/common/git/.gitconfig $REMOTE:~/.gitconfig

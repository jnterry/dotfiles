#!/usr/bin/env bash
#
# Deletes an existing compiled emacs list (elc files) and then rebuilds them

set -e

pushd $(dirname "${0}") > /dev/null
SCRIPTDIR=$(pwd -L)
popd > /dev/null

# Delete any existing compiled .elc files
if [[ `find ${SCRIPTDIR}/config | grep .elc` ]] ; then
		rm `find ${SCRIPTDIR}/config | grep .elc`
fi

# Refresh the emacs custom file
mkdir -p ~/.emacs.d/
rm -f ~/.emacs.d/custom.el
touch ~/.emacs.d/custom.el

# Make emacs load the config file - this will return 0 if the config could
# be loaded, but non 0 if an error occurs
emacs --version # print for debugging purposes
emacs --batch -l "${SCRIPTDIR}/init.el"

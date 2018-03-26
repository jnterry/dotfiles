#!/usr/bin/env bash
#
# Deletes an existing compiled emacs list (elc files) and then rebuilds them

set -e

pushd $(dirname "${0}") > /dev/null
SCRIPTDIR=$(pwd -L)
popd > /dev/null

if [[ `find ${SCRIPTDIR}/config | grep .elc` ]] ; then
		rm `find ${SCRIPTDIR}/config | grep .elc`
fi

rm    ${SCRIPTDIR}/custom.el
touch ${SCRIPTDIR}/custom.el

emacs --batch \
			-l "${SCRIPTDIR}/init.el"

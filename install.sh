#!/usr/bin/env bash
#
# Installs all dotfiles to the appropriate locations

pushd $(dirname "${0}") > /dev/null
SCRIPTDIR=$(pwd -L)
popd > /dev/null

STOW_CMD='/usr/bin/stow'
if [[ -f '/usr/bin/stowforce' ]] ; then
	STOW_CMD='/usr/bin/stowforce'
fi

#######################################
# Install common files
cd ${SCRIPTDIR}/common
for d in `ls -d */`;
do	
	printf "Installing %32s : " $d
	$STOW_CMD $d --target $HOME
	printf "Done\n"
done

#######################################
# Install host specific files
HOSTDIR=${SCRIPTDIR}/$(hostname)
if [[ -d ${HOSTDIR} ]] ; then
	cd ${HOSTDIR}
	for d in `ls -d */`;
	do
		printf "Installing %32s : " $d
		$STOW_CMD $d --target $HOME
		printf "Done\n"
	done
fi

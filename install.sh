#!/bin/sh
#
# Installs all dotfiles to the appropriate locations

SCRIPT=$(readlink -f "$0")
SCRIPTDIR=$(dirname $SCRIPT)

# We don't want to rely on $HOME being set as this may be run as part of
# automatic system setup when enviroment variables are not present
#
# Hence we are going to detect the home directory of the user this script
# is being ran as, and install to there
TARGET=$(eval echo "~$(whoami)")

STOW_CMD='/usr/bin/stow'
if [ -f '/usr/bin/stowforce' ] ; then
	STOW_CMD='/usr/bin/stowforce'
fi

#######################################
# Install common files
cd ${SCRIPTDIR}/common
for d in `ls -d */`;
do
	printf "Installing %32s : " $d
	$STOW_CMD $d --target $TARGET
	printf "Done\n"
done

#######################################
# Install host specific files
HOSTDIR=${SCRIPTDIR}/$(hostname)
if [ -d ${HOSTDIR} ] ; then
	cd ${HOSTDIR}
	for d in `ls -d */`;
	do
		printf "Installing %32s : " $d
		$STOW_CMD $d --target $TARGET
		printf "Done\n"
	done
fi

exit 0

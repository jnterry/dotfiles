#!/bin/bash
#
# Installs all dotfiles to the appropriate locations

# We don't want to rely on $PATH being set as this may be run as part of
# automatic system setup when enviroment variables are not present
export PATH="$PATH:/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin/"

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

#####################
# Helper to install files from a particular directory
install_from_dir () {
	pushd ${1}
	for d in `ls -d */`;
	do
		printf "Installing %32s : '${d}' to '${TARGET}'"
		$STOW_CMD $d --target $TARGET
		printf "Done\n"
	done
	popd
}

# Install common files
install_from_dir ${SCRIPTDIR}/config/common

# Install host specific files
HOSTDIR=${SCRIPTDIR}/config/$(hostname)
echo $HOSTDIR
if [ -d ${HOSTDIR} ] ; then
	install_from_dir $HOSTDIR
fi

exit 0

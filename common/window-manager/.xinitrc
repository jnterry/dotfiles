#!/usr/bin/env bash

# Load machine specific settings
if [ -f ~/.xprofile ] ; then
	. ~/.xprofile
fi

# Start window manager
/usr/bin/i3

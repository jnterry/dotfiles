# The aim of this file is to provide some "smart" commands for editing files
# which pick the "best" editor currently available

# First off, it if the emacs terminal server is not running the start it

if [[ -e /usr/bin/emacs && ! -e /tmp/emacs1000/terminal ]] ; then
		echo "Starting emacs server in background..."
		# Do in brackets (subshell) to prevent print of pid etc
		(emacs --daemon=terminal > /dev/null 2> /dev/null &)
fi

# The et function (edit-terminal) will try to edit a file in the
# terminal emacs server, or in vim then vi if emacs is not installed
function et() {

		if [[ ! -e /usr/bin/emacs ]] ; then
				if [[ ! -e /usr/bin/vim ]] ; then
						vi "$@"
				else
						vim "$@"
				fi
				return
		fi

		# --alternate-editor="" means if the server is not running
		# then start it and connect, rather than falling back to vim or
		# similar
		emacsclient -nw -s "terminal" --alternate-editor=""  "$@"
}

# The e function (edit) will try to edit a file in the gui emacs
# server. This starts automatically whenever a gui copy of emacs
# is launched, and the server is not already running
#
# If gui emacs server is not running then this falls back to editing
# in terminal emacs (which in turn will fall back to vim if emacs
# is not installed, and vi as a last resort)
function e() {

		if [[ -e /tmp/emacs1000/gui ]] ; then
				# If the gui server is running then open file there
				emacsclient -s "gui" "$@"
		else
				# Otherwise open in the terminal emacs server
				et "$@"
		fi
}

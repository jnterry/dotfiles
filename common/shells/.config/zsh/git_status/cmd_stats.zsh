# This file prints stats after a command runs

function preexec() {
		echo "About to run command"
}

function precmd() {
		echo "Just finished command"
}

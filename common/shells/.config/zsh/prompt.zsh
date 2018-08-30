# Config for the prompt of zsh
#
# In order for this to render correctly, you will need a
# [Powerline-patched font](https://github.com/Lokaltog/powerline-fonts).
#
# The Prompt is the text that is printed before the user types a command
# into the shell, for example, a fairly standard format is:
# user@host:/path/to/cwd$
#
# zsh will use whatever is defined in the PROMPT env_var as the string
#
# The first step is to ensure that zsh will dynamically evaluate strings such
# as $(cmd):
setopt PROMPT_SUBST

# Now we can just say the prompt is to run our own function
# Not the single quotes so this is evaluated each time the prompt is
# displayed rather than right now
PROMPT='$(print_prompt)'

print_prompt() {
		# Capture the exit value of the command that has just run, this is going to
		# get overwritten before we want to display it
		LAST_EXIT=$?

		# General layout of the prompt to produce is

		#   LEFT_STATUS_INFO                         RIGHT_STATUS_INFO
		#   username >

		# To avoid having to calculate the length of the LEFT_STATUS_INFO
		# to pad out the gap before the right we will...

		#   1. Print the right info first
		print_top_right

		#   2. Return cursor to start of line, but not start a new line
		printf "\r"

		#   3. Print the left text
		print_top_left ${LAST_EXIT}

		# Finally go to next line, and print actual prompt for command
		printf "\n"
		printf "$(whoami) ❯"
}

print_top_right() {
		GIT_STATUS=$(git status 2&> /dev/null)

		if [[ $? != 0 ]] ; then
				return
		fi

		GIT_BRANCH=$(echo ${GIT_STATUS} | grep "^On branch .*$" | awk '{ print $3 }')

		GIT_BRANCH_LENGTH=$(printf ${GIT_BRANCH} | wc -c)

		# Move cursor to end of line, minus a bit for the branch name
		COLS=$(tput cols)
		printf "%*s" $((COLS-GIT_BRANCH_LENGTH))

		printf ${GIT_BRANCH}

		printf "\r"
}

print_top_left () {
		#################################

		local LC_ALL="" LC_CTYPE="en_US.UTF-8"
		# This is a full height filled > character, used to divide segments
		# This codepoint will only render correctly using a powerline font after 2012
		SEGMENT_SEPARATOR=$'\ue0b0'

		tput setab 18
		tput setaf 7
		printf " $(hostname) "

		tput setab 31
		tput setaf 18
		printf ${SEGMENT_SEPARATOR}

		if [[ ${LAST_EXIT} == 0 ]] ; then
				tput setaf 7 # white
		else
				tput bold
				#tput blink
				tput setaf 1 # red
		fi


		printf " %3s " ${LAST_EXIT}

		tput sgr0

		tput setab 117
		tput setaf 31
		printf ${SEGMENT_SEPARATOR}

		tput setaf 0

		printf " $(pwd | sed 's:^$HOME:~:') "

		tput sgr0
		tput setaf 117
		printf ${SEGMENT_SEPARATOR}

		# disable all colors
	  tput sgr0
}

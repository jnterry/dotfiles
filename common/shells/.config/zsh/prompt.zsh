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

function print_prompt() {
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
		printf "$(whoami) ❯ "
}

function print_top_right() {
		# Top right status is info about the current git repository
		#
		# Output format is basically exactly that of:
		# https://github.com/olivierverdier/zsh-git-prompt
		#
		# ...but all this code is self done, as zsh-git-prompt can't be aligned
		# right correctly, since it contains format characters that make the length
		# of the string in characters be different to the length visually,
		# hence I've written my own code that tracks the visual length seperately
		# also that repository appears to be unmaintained, and include wierd output
		# with recent versions of git
		#
		# !!!!!!!!!!!!!!!!!!!!!!!!!!
		#
		#  Tested with git v 2.17.1
		#
		# !!!!!!!!!!!!!!!!!!!!!!!!!!

		# collect the data we need...
		GIT_STATUS=$(git status 2&> /dev/null)

		# If error then we're not in a git repo, just quit
		if [[ $? != 0 ]] ; then
				return
		fi

		# One issue is we need to know how long the status string is so we can
		# go to the end of the line, move left N characters, then print the string
		# However we can't just count the length of the string at the end, since it
		# will include characters for formatting colors, bold, etc
		#
		# Instead we will keep track of the current FULL_TEXT and FULL_LENGTH, and
		# build it up as we go, then print it right at the end
		FULL_TEXT=""
		FULL_LENGTH=0

		########################################################
		# Extract the branch name / commit sha
		GIT_BRANCH=$(echo ${GIT_STATUS} | grep "^On branch .*$" | awk '{ print $3 }')

		if [[ ! -z ${GIT_BRANCH} ]] ; then
				FULL_TEXT=${FULL_TEXT}"\e[95m\e[1m"${GIT_BRANCH}"\e[0m"
				GIT_BRANCH_LENGTH=$(echo ${GIT_BRANCH} | wc -c)
				FULL_LENGTH=$((FULL_LENGTH + GIT_BRANCH_LENGTH))

				ON_BRANCH=1
		else # Check if we are in detached head mode rather than on a branch
				GIT_BRANCH=":"$(echo ${GIT_STATUS} | grep "HEAD detached .*$" | awk '{ print $4 }')

				if [[ ! -z ${GIT_BRANCH} ]] ; then
						FULL_TEXT=${FULL_TEXT}"\e[93m\e[1m"${GIT_BRANCH}"\e[0m"
						GIT_BRANCH_LENGTH=$(echo ${GIT_BRANCH} | wc -c)
						FULL_LENGTH=$((FULL_LENGTH + GIT_BRANCH_LENGTH))

						ON_BRANCH=0
				fi
		fi
		########################################################


		########################################################
		# Determine state of local compared to remote
		if [[ ${ON_BRANCH} == 1 ]] ; then

				# Your branch is begind of 'origin/master' by 1 commit.
				TREE_INFO=$( echo ${GIT_STATUS} | grep "^Your branch is behind")
				if [[ ! -z ${TREE_INFO} ]] ; then
						COMMITS_BEHIND=$(echo ${TREE_INFO} | awk '{ print $7 }')
				else
						# Your branch is ahead of 'origin/master' by 1 commit.
						TREE_INFO=$( echo ${GIT_STATUS} | grep "^Your branch is ahead of")

						if [[ ! -z ${TREE_INFO} ]] ; then
								COMMITS_AHEAD=$(echo ${TREE_INFO} | awk '{ print $8 }')
						else
								# Your branch and 'origin/master' have diverged,
								# and have 1 and 1 different commits each, respectively.

								TREE_INFO=$( echo ${GIT_STATUS} | grep "^and have [0-9]* and [0-9]* different commits each")

								if [[ ! -z ${TREE_INFO} ]] ; then
										COMMITS_AHEAD=$(echo ${TREE_INFO} | awk '{ print $3 }')
										COMMITS_BEHIND=$(echo ${TREE_INFO} | awk '{ print $5 }')
								fi
						fi
				fi

				# if [[ ! -z "${COMMITS_BEHIND}${COMMITS_AHEAD}" ]] ; then
				# 		# If we are ahead or behind a space after branch name before
				# 		# the remote state indicator block
				# 		FULL_TEXT="${FULL_TEXT} "
				# 		FULL_LENGTH=$((FULL_LENGTH + 1))
				# fi

				if [[ ! -z ${COMMITS_BEHIND} ]] ; then
						BEHIND_LENGTH=$(echo ${COMMITS_BEHIND} | wc -c)

						FULL_TEXT=${FULL_TEXT}"↓"${COMMITS_BEHIND}
						FULL_LENGTH=$((FULL_LENGTH + BEHIND_LENGTH))
				fi

				if [[ ! -z ${COMMITS_AHEAD} ]] ; then
						AHEAD_LENGTH=$(echo ${COMMITS_AHEAD} | wc -c)

						FULL_TEXT=${FULL_TEXT}"↑"${COMMITS_AHEAD}
						FULL_LENGTH=$((FULL_LENGTH + AHEAD_LENGTH))
				fi
		fi
		########################################################

		FULL_TEXT=${FULL_TEXT}"|"
		FULL_LENGTH=$((FULL_LENGTH + 1))

		########################################################
		# Determine state of local copy

		echo ${GIT_STATUS} | grep 'working tree clean' > /dev/null 2> /dev/null

		if [[ $? == 0 ]] ; then
				FULL_TEXT=${FULL_TEXT}'\033[1;32m✔\033[0m'
				FULL_LENGTH=$((FULL_LENGTH + 1))
		else
				# its much easier to parse the git short status than full git status,
				# do lets do that...
				SHORT_STATUS=$(git status --short)

				FILES_STAGED=$(echo ${SHORT_STATUS} | awk '{print substr($0, 1, 1) }' | tr -d " \n" | tr -d "?" | wc -c)
				FILES_CHANGED=$(echo ${SHORT_STATUS} | awk '{print substr($0, 2, 2) }' | tr -d " \n" | tr -d "?" | wc -c)
				FILES_UNTRACKED=$(echo ${SHORT_STATUS} | awk '{print substr($0, 2, 2) }' | tr -d "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" | tr -d " \n" | wc -c)

				if [[ ! ${FILES_STAGED} == 0 ]] ; then
						STATE="● ${FILES_STAGED}"
						LENGTH=$(echo ${STATE} | wc -c)

						FULL_TEXT=${FULL_TEXT}"\033[1;32m"${STATE}"\033[0m"
					  FULL_LENGTH=$((LENGTH + FULL_LENGTH - 3)) # -3 as ● is multicharcter unicode
				fi

				if [[ ! ${FILES_CHANGED} == 0 ]] ; then
						STATE="✚ ${FILES_CHANGED}"
						LENGTH=$(echo ${STATE} | wc -c)

						FULL_TEXT=${FULL_TEXT}"\033[1;33m"${STATE}"\033[0m"
					  FULL_LENGTH=$((LENGTH + FULL_LENGTH - 3)) # -3 as ✚ is multicharcter unicode
				fi

				if [[ ! ${FILES_UNTRACKED} == 0 ]] ; then
						STATE="… ${FILES_UNTRACKED}"
						LENGTH=$(echo ${STATE} | wc -c)

						FULL_TEXT=${FULL_TEXT}"\033[1;31m"${STATE}"\033[0m"
					  FULL_LENGTH=$((LENGTH + FULL_LENGTH - 3)) # -3 as … is multicharcter unicode
				fi
		fi

		# We assume that cursor is current at start of line, move to the end
		# of line minus what
		# Move cursor to end of line, minus a bit for the branch name
		COLS=$(tput cols)
		printf "%*s" $((COLS - FULL_LENGTH + 1))

		printf ${FULL_TEXT}

		printf "\r"
}

function print_top_left () {
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

		printf " $(pwd | sed 's/^\/home\/jamie/~/') "

		tput sgr0
		tput setaf 117
		printf ${SEGMENT_SEPARATOR}

		# disable all colors
	  tput sgr0
}

# Config for the prompt of zsh, adapted from:
#
# In order for this to render correctly, you will need a
# [Powerline-patched font](https://github.com/Lokaltog/powerline-fonts).
#
#
# The Prompt is the text that is printed before the user types a command
# into the shell, for example, a fairly standard format is:
# user@host:/path/to/cwd$
#
# zsh will use whatever is defined in the PROMPT env_var as the string,
# This script populates that variable with a different string so that the prompt
# is a little more interseting

# Firstly, ensure zsh performs string substitution on the value of ${PROMPT}
# This is required so the PROMPT string can contain commands (eg, wrapped in
# $(...)), these will be ran each time the prompt is shown for dynamic content.
# For example, printing the current directory after changing to a new one
setopt PROMPT_SUBST

##########################################################
# Segment drawing - A few utility functions to make it easy
# and re-usable to draw segmented prompts
CURRENT_BG='NONE'

# Special Powerline characters
() {
		local LC_ALL="" LC_CTYPE="en_US.UTF-8"
		# This is a full height filled > character, used to divide segments
		# This codepoint will only render correctly using a powerline font after 2012
		SEGMENT_SEPARATOR=$'\ue0b0'
}

# Begin a segment
# Takes two arguments, background and foreground. Both can be omitted,
# rendering default background/foreground.
prompt_segment() {
  local bg fg
  [[ -n $1 ]] && bg="%K{$1}" || bg="%k"
  [[ -n $2 ]] && fg="%F{$2}" || fg="%f"
	if [[ $CURRENT_BG != 'NONE' && $1 != $CURRENT_BG ]]; then
			echo -n "%{$bg%F{$CURRENT_BG}%}$SEGMENT_SEPARATOR%{$fg%}"
	else
			echo -n "%{$bg%}%{$fg%} "
	fi
	[[ -n $3 ]] && echo -n $3
	CURRENT_BG=$1
}

# End the prompt, closing any open segments
prompt_end() {
  if [[ -n $CURRENT_BG ]]; then
    echo -n " %{%k%F{$CURRENT_BG}%}$SEGMENT_SEPARATOR"
  else
    echo -n "%{%k%}"
  fi
  echo -n "%{%f%}"
  CURRENT_BG=''
}

##########################################################
# Prompt components - these define the different segments for the prompt

# Prints out the hostname
prompt_host() {

		text_color=white
		# :TODO: ideally we would have difference between workstations and servers, but how to detect???
		if [[ $IS_SERVER ]] ; then
				text_color=red
		fi
		prompt_segment 18 text_color "$(hostname) "
}

prompt_dir() {
		# We don't want super long path names
		#
		# Instead we will show a simple indicator (in $HOME or not) followed by the
		# last component of the path

		# The home inidcator will print:
		# ~ if cwd is subdirectory of $HOME
		# / if cwd is subdirectory of root
		#              get cwd   replace $HOME with ~     replace anything other than just ~
		root_indicator='`pwd    | sed "s:^$HOME.*:~:"   |  sed "s:^[^~].*$:/:"`'

		# Get the last component of the path
		# But if we are in home or root dont print anything,
		# since thats already shown by root_indicator
		#basename_indicator='`basename "$(pwd | sed "s:^$HOME$::" | sed "s:^/$::")"`'
		basename_indicator='`pwd | sed "s:^${HOME}/?::" | sed "s:^/$::"`'

		prompt_segment 31  white " $root_indicator "
		prompt_segment 117 black "$basename_indicator"
}

prompt_git() {
		branch_name='`git status 2> /dev/null | grep -m 1 "On branch " | cut -c 11- | sed "s:[ \t]*$::"`'

		echo "$branch_name"
}

# Status:
# - was there an error
# - am I root
# - are there background jobs?
prompt_status() {
  local symbols
  symbols=()
  [[ $RETVAL -ne 0 ]] && symbols+="%{%F{red}%}✘"
  [[ $UID -eq 0 ]] && symbols+="%{%F{yellow}%}⚡"
  [[ $(jobs -l | wc -l) -gt 0 ]] && symbols+="%{%F{cyan}%}⚙"

  [[ -n "$symbols" ]] && prompt_segment black default "$symbols"
}

## Main prompt
build_title_left() {
		#RETVAL=$?
		#prompt_status
		#prompt_virtualenv
		#prompt_context
		#prompt_dir
		#prompt_git
		#prompt_bzr
		#prompt_hg
		#prompt_end

		prompt_host
		prompt_dir
		prompt_end
}

build_title_right() {
	  prompt_git
}

build_prompt() {
		printf '$(tput bold)$(whoami) ❯$(tput sgr0)'
}


print_prompt() {
	  echo "\$(printf '%*s\r%s\n%s' \$(tput cols) \"$(build_title_right)\" \"$(build_title_left)\" \"$(build_prompt)\")"
}

PROMPT="$(print_prompt) "

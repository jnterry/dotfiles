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

source ~/.config/zsh/git_status/zshrc.sh

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

prompt_jobs() {
		prompt_segment 31 white " jobs: \$(jobs -l | wc -l) "
}

prompt_dir() {
		# Write directory name, but replace a leading $HOME path with ~
		prompt_segment 117 black " \$(pwd | sed 's:^$HOME:~:') "
}

prompt_git() {
		branch_name='`git status 2> /dev/null | grep -m 1 "On branch " | cut -c 11- | sed "s:[ \t]*$::"`'

		echo "$branch_name"
}

## Main prompt
build_title_left() {
		prompt_host
		prompt_jobs
		prompt_dir
		prompt_end
}

build_title_right() {
	  echo '$(git_super_status)'
}

build_prompt() {
		printf '$(whoami) ❯'
}


print_prompt() {
		# Here we print the prompt
		# The trick to getting some text right aligned is:
		# 1. Print the right text full width
		#    - This is done with %*s -> * means width is specified by another argument
		#    - which we fill in with the total number of columns, determined with "tput cols"
		# 2. Use \r (caridge return) to go back to start of line (but not to the next line)
		# 3. Print the left text
		#
	  echo "\$(printf '%*s\r%s\n%s' \$(tput cols) \"$(build_title_right)\" \"$(build_title_left)\" \"$(build_prompt)\")"

		# :TODO: the git status text is currently not completely right aligned -
		# but is instead just near the right (but by varing amounts, so can't
		# correct with a constant)
		# I think this is due to formatting characters taking up "space" in terms of
		# character count but not visually.
		# Theoretically can wrap in %{...%} to make zsh think it takes 0 width, but
		# that seems to be broken... (maybe its not being escaped into the final string?)
		# https://unix.stackexchange.com/a/90876

		# can also set RPROMPT to have zsh auto print something on the right, but
		# that seems to suffer from the same problem of format characters taking up space
		# Additionally it is printed on the second line when LHS prompt contains a new-line
		# character (as ours does)
}

PROMPT="$(print_prompt) "

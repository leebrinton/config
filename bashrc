##### vim:set filetype=sh : ###################################################
#
# ~/config/bashrc 
#
# Executed by all bash shells
#
###############################################################################

#set -v
#------------------------------------------------------------------------------
# Limit core dumps to zero bytes
#------------------------------------------------------------------------------
#ulimit -c 0

#------------------------------------------------------------------------------
# If not running interactively, don't do anything
#------------------------------------------------------------------------------
[[ "$-" != *i* ]] && return

#------------------------------------------------------------------------------
# Initialize ANSI escape sequences for colors
#------------------------------------------------------------------------------
if [[ -f "${HOME}/bin/ansicolor.sh" ]]
then
  source "${HOME}/bin/ansicolor.sh"

  initializeANSI
fi

#------------------------------------------------------------------------------
# Include korn-common-startup
#------------------------------------------------------------------------------
if [[ -f "${HOME}/config/korn_common_startup" ]]
then
    source "${HOME}/config/korn_common_startup"
fi

#------------------------------------------------------------------------------
# include user defined functions
#------------------------------------------------------------------------------
if [[ -f "${HOME}/config/bash_functions" ]]
then
    source "${HOME}/config/bash_functions"
fi

#------------------------------------------------------------------------------
# Support cd to a variable
#------------------------------------------------------------------------------
shopt -s cdable_vars

#------------------------------------------------------------------------------
# Support fixing minor spelling errors in directory component of cd command 
#------------------------------------------------------------------------------
shopt -s cdspell

#------------------------------------------------------------------------------
# No need to check to see if hashed program exists (use rehash if needed)
#------------------------------------------------------------------------------
shopt -u checkhash

#------------------------------------------------------------------------------
# Make bash check it's window size after a process completes
#------------------------------------------------------------------------------
shopt -s checkwinsize

#------------------------------------------------------------------------------
# Save all lines of a multi-line command in one history entry
#------------------------------------------------------------------------------
shopt -s cmdhist

#------------------------------------------------------------------------------
# Don't include filenames beginning with . in results of filename expansion
#------------------------------------------------------------------------------
shopt -u dotglob

#------------------------------------------------------------------------------
# Exit if non-interactive shell exec fails
#------------------------------------------------------------------------------
shopt -u execfail

#------------------------------------------------------------------------------
# Enable aliases
#------------------------------------------------------------------------------
shopt -s expand_aliases

#------------------------------------------------------------------------------
# Enable extended globing
#------------------------------------------------------------------------------
shopt -s extglob

#------------------------------------------------------------------------------
# Append to the history file instead of overwriting it
#------------------------------------------------------------------------------
shopt -s histappend

#------------------------------------------------------------------------------
# Allow editing of history expansion
#------------------------------------------------------------------------------
shopt -u histreedit
shopt -s histverify

#------------------------------------------------------------------------------
# Attempt to complete hostnames
#------------------------------------------------------------------------------
shopt -s hostcomplete

#------------------------------------------------------------------------------
# Do not set SIGHUP to all jobs on exit
#------------------------------------------------------------------------------
shopt -u huponexit

#------------------------------------------------------------------------------
# Allow interactive comments
#------------------------------------------------------------------------------
shopt -s interactive_comments

#------------------------------------------------------------------------------
# If possible replace newlines with ; in history
#------------------------------------------------------------------------------
shopt -u lithist

#------------------------------------------------------------------------------
# Do not warn when mail has been read
#------------------------------------------------------------------------------
shopt -u mailwarn

#------------------------------------------------------------------------------
# Do not attempt command completion with an empty line
#------------------------------------------------------------------------------
shopt -s no_empty_cmd_completion

#------------------------------------------------------------------------------
# Use case sensitive Globbing
#------------------------------------------------------------------------------
shopt -u nocaseglob

#------------------------------------------------------------------------------
# Do not expand filename patterns with no matchs to the null string
#------------------------------------------------------------------------------
shopt -u nullglob
shopt -s failglob

#------------------------------------------------------------------------------
# Enable programmable completion
#------------------------------------------------------------------------------
shopt -s progcomp

#------------------------------------------------------------------------------
# Enable expansion of prompt variables
#------------------------------------------------------------------------------
shopt -s promptvars

#------------------------------------------------------------------------------
# Disable error message when shift exceeds the parameters
#------------------------------------------------------------------------------
shopt -u shift_verbose

#------------------------------------------------------------------------------
# Enable finding sourced file on the PATH
#------------------------------------------------------------------------------
shopt -s sourcepath

#------------------------------------------------------------------------------
# No expansion of backslash-escapes
#------------------------------------------------------------------------------
shopt -u xpg_echo

#------------------------------------------------------------------------------
# Do not allow use of a single CTRL-D to log off
#------------------------------------------------------------------------------
set -o ignoreeof

#------------------------------------------------------------------------------
# Let me know when background jobs complete
#------------------------------------------------------------------------------
set -o notify

#------------------------------------------------------------------------------
# Use emacs style line editing interface
#------------------------------------------------------------------------------
set -o emacs

case $TERM in
  cygwin|linux)
    bind '"\e[1~":beginning-of-line'  # Home key
    bind '"\e[4~":end-of-line'        # End key
    ;;
  *xterm*|rxvt|aterm|dtterm|kterm|Eterm)
    bind '"\eOH":beginning-of-line'
    bind '"\eOF":end-of-line'
    ;;
esac

bind '"\e[2~":overwrite-mode'             # Insert key
bind '"\e[3~":delete-char'                # Delete key
bind '"\e[5~":history-search-forward'     # PageUp key
bind '"\e[6~":history-search-backward'    # PageDown key

bind '"\er":history-search-backward'
bind '"\ef":history-search-forward'

bind '"\C-x\C-x": exchange-point-and-mark'
bind '"\M-w": copy-region-as-kill'

#------------------------------------------------------------------------------
# Enable job control
#------------------------------------------------------------------------------
set -o monitor

###############################################################################
#
# Set options for command line history
#
###############################################################################
#------------------------------------------------------------------------------
# If the current command is the same as the last
# then don't add an entry to the history file 
#------------------------------------------------------------------------------
HISTCONTROL=ignoreboth:erasedups

# Ignore some controlling instructions
# HISTIGNORE is a colon-delimited list of patterns which should be excluded.
# The '&' is a special pattern which suppresses duplicate entries.
export HISTIGNORE=$'[ \t]*:&:[fb]g:exit:ls'

#------------------------------------------------------------------------------
# Set the history file
#------------------------------------------------------------------------------
HISTFILE=~/.bash_history

#------------------------------------------------------------------------------
# Set the size of the history file
#------------------------------------------------------------------------------
HISTFILESIZE=$HISTSIZE

#------------------------------------------------------------------------------
# Read in history from file
#------------------------------------------------------------------------------
history -r

export HISTCONTROL HISTFILE HISTFILESIZE


#------------------------------------------------------------------------------
# Set the main prompt variable 
#------------------------------------------------------------------------------
git_prompt_cmd='git_branch'

if [[ -f /opt/local/share/git/git-prompt.sh ]]
then
    source /opt/local/share/git/git-prompt.sh
    export GIT_PS1_SHOWDIRTYSTATE=1
    export GIT_PS1_SHOWSTASHSTATE=1
    export GIT_PS1_SHOWUNTRACKEDFILES=1
    # If you would like to see the difference between HEAD and its upstream,
    # set GIT_PS1_SHOWUPSTREAM="auto".  A "<" indicates you are behind, ">"
    # indicates you are ahead, "<>" indicates you have diverged and "="
    # indicates that there is no difference. You can further control
    # behaviour by setting GIT_PS1_SHOWUPSTREAM to a space-separated list
    # of values:
    #
    #     verbose       show number of commits ahead/behind (+/-) upstream
    #     name          if verbose, then also show the upstream abbrev name
    #     legacy        don't use the '--count' option available in recent
    #                   versions of git-rev-list
    #     git           always compare HEAD to @{upstream}
    #     svn           always compare HEAD to your SVN upstream
    #
    # Once you have set GIT_PS1_SHOWUPSTREAM, you can override it on a
    # per-repository basis by setting the bash.showUpstream config variable.
    export GIT_PS1_SHOWUPSTREAM='auto'

    # If you would like to see more information about the identity of
    # commits checked out as a detached HEAD, set GIT_PS1_DESCRIBE_STYLE
    # to one of these values:
    #
    #     contains      relative to newer annotated tag (v1.6.3.2~35)
    #     branch        relative to newer tag or branch (master~4)
    #     describe      relative to older annotated tag (v1.6.3.1-13-gdd42c2f)
    #     tag           relative to any older tag (v1.6.3.1-13-gdd42c2f)
    #     default       exactly matching tag
    export GIT_PS1_DESCRIBE_STYLE='branch'

    # If you would like a colored hint about the current dirty state, set
    # GIT_PS1_SHOWCOLORHINTS to a nonempty value. The colors are based on
    # the colored output of "git status -sb" and are available only when
    # using __git_ps1 for PROMPT_COMMAND or precmd.
    # export GIT_PS1_SHOWCOLORHINTS=1

    # If you would like __git_ps1 to do nothing in the case when the current
    # directory is set up to be ignored by git, then set
    # GIT_PS1_HIDE_IF_PWD_IGNORED to a nonempty value. Override this on the
    # repository level by setting bash.hideIfPwdIgnored to "false".
    # export GIT_PS1_HIDE_IF_PWD_IGNORED=1

    git_prompt_cmd='__git_ps1 "(%s)"'
fi

 [[ "$RUNNING_MSYS2_BASED_ENV" != 'true' ]] && \
     PS1='\[${greenf}\]\u@\h \[${yellowf}\]\w \[${purplef}\]$(eval $git_prompt_cmd)\[${reset}\] (\!)\n\$ '
     
#     PS1='\[${greenf}\]\u@\h \[${yellowf}\]\w \[${cyanf}\]\D{%a %b %d %I:%M:%S} \[${purplef}\]$(eval $git_prompt_cmd)\[${reset}\] (\!)\n\$ '

#------------------------------------------------------------------------------
# Set the PROMPT_COMMAND variable 
#------------------------------------------------------------------------------
if [[ $TERM != 'linux' && $TERM != 'vt220' && $RUNNING_MINGW != 'true' ]]
then
  X='\033]2;'
  Y="$TERM: $HOSTNAME: $PWD"
  Z="\007\033]1;$PWD\007"
  TITLE="$X$Y$Z"
  PROMPT_COMMAND='echo -ne "$TITLE"'
fi

#==============================================================================
# Enable programmable completion
#==============================================================================
#------------------------------------------------------------------------------
# Define to avoid stripping description in
# --option=description of './configure --help'
#------------------------------------------------------------------------------
COMP_CONFIGURE_HINTS=1

#------------------------------------------------------------------------------
# Define to avoid flattening internal contents of tar files
#------------------------------------------------------------------------------
COMP_TAR_INTERNAL_PATHS=1

if [[ -f /usr/share/bash-completion/bash_completion ]]
then
  BASH_COMPLETION=/usr/share/bash-completion/bash_completion
elif [[ -f /etc/bash_completion ]]
then
  BASH_COMPLETION=/etc/bash_completion
elif [[ -f /usr/local/etc/bash_completion ]]
then
  BASH_COMPLETION=/usr/local/etc/bash_completion

  if [[ -d /usr/local/etc/bash_competion.d ]]
  then
    BASH_COMPLETION_DIR=/usr/local/etc/bash_completion.d
  fi
elif [[ -f /opt/local/etc/bash_completion ]]
then
  BASH_COMPLETION=/opt/local/etc/bash_completion
fi

if [[ -f /opt/local/share/git/completion/git-completion.bash ]]
then
    source /opt/local/share/git/completion/git-completion.bash
fi

# If this shell is interactive, turn on programmable completion enhancements.
# Any completions you add in ~/.bash_completion are sourced last.
case $- in
  *i*) [[ -n $BASH_COMPLETION ]] && . $BASH_COMPLETION ;;
esac

#------------------------------------------------------------------------------
# Aliases
#------------------------------------------------------------------------------
if [[ -f "${HOME}/config/bash_aliases" ]]
then
  source "${HOME}/config/bash_aliases"
fi

if [[ -x /usr/bin/mint-fortune ]]
then
  /usr/bin/mint-fortune
fi

# THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="${HOME}/.sdkman"
if [[ -s "${SDKMAN_DIR}/bin/sdkman-init.sh" ]]
then
  source "${SDKMAN_DIR}/bin/sdkman-init.sh"
fi

###############################################################################
# End of ~/config/bashrc
###############################################################################

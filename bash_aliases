##### vim:set filetype=sh : ###################################################
# 
# bash_aliases
#
###############################################################################
#------------------------------------------------------------------------------
# Include korn shell common aliases
#------------------------------------------------------------------------------
if [ -f ~/config/korn_common_aliases ]
then
	source ~/config/korn_common_aliases ]
fi

#------------------------------------------------------------------------------
# Korn shell command emulation
#------------------------------------------------------------------------------
alias p='popd'                  # Pop the directory stack 
alias r='fc -s'                 # Repeat last command
alias rehash='hash -r'          # Forget all hashed program locations
alias wh='type -all'
alias viewman='groff -t -man -Tascii'

#------------------------------------------------------------------------------
# If we are inside a Neovim terminal and make an attempt run nvim use nvr
# instead.  This will open the file using the nvim that contains the terminal.
#------------------------------------------------------------------------------
if [ -n "$NVIM_LISTEN_ADDRESS" ]
then
    if [ -x "$(command -v nvr)" ]
    then
        alias nvim=nvr
    else
        alias nvim='echo "No nesting!"'
    fi
fi

###############################################################################
# end of ~/config/bash_aliases
###############################################################################


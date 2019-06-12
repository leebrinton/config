##### vim:set filetype=sh : ##################################################
# kshrc
##############################################################################
if [[ -f ~/bin/ansicolor.sh ]]
then
  . ~/bin/ansicolor.sh

  initializeANSI
fi

#-----------------------------------------------------------------------------
# Include Korn common startup
#-----------------------------------------------------------------------------
if [[ -f ~/config/korn_common_startup ]]
then
  . ~/config/korn_common_startup
fi

SHELL=/bin/ksh
PS1='${USER:=${USERNAME:=${LOGNAME}}}: ${PWD} (!) \$ '

FCEDIT="${EDITOR}"
HISTEDIT="${EDITOR}"
DIRSTACK=""
export DIRSTACK

#
# The list of directories we will attempt to auto load  functions from
# FPATH=

set -o emacs
set -o ignoreeof
set -o markdirs

bind '[1~'=beginning-of-line
bind '[3~'=eot-or-delete
bind '[4~'=end-of-line
bind '	'=complete

#-----------------------------------------------------------------------------
# Include Korn common aliases
#-----------------------------------------------------------------------------
if [[ -f ~/config/korn_common_aliases ]]
then
  . ~/config/korn_common_aliases
fi

#-----------------------------------------------------------------------------
# Korn functions
#-----------------------------------------------------------------------------
function emacs {
  typeset prg
  
  if [ "${RUNNING_CYGWIN}" = 'true' ]
  then
    prg="${EMACSHOME}/bin/runemacs"
  elif [ "${RUNNING_DARWIN}" = 'true' ]
  then
    prg=/Applications/Emacs.app/Contents/MacOS/Emacs
  else
    prg=emacs
  fi

  command $prg -bg 'light yellow' -cr salmon -g 80x50 $@
}

#---------------------------------------------------------------------------
# findsrc - find source files
#
#   Find all java, xml and sql files in the current directory and children
#
#  Another way to do this:
#    find . -type f -a 
#      \( -name '*.java' -o -name '*.xml' -o -name '*.sql' \) -print
#---------------------------------------------------------------------------
function findsrc {
  if [ "${RUNNING_DARWIN}" = 'true' ]
  then
    find . -name .svn -prune -or \
           \( -type f -and \
           \( -name '*.java' -or -name '*.xml' -or -name '*.sql' \) \) -print
  else
    find . -type f -a -regex '.*\.\(java\|xml\|sql\)$' -print
  fi
}

#---------------------------------------------------------------------------
# srcsearch - find source files and search them for an regex
#---------------------------------------------------------------------------
function srcsearch {
  findsrc | xargs grep -i ${1}
}

#---------------------------------------------------------------------------
# Find process to kill and kill it.
#---------------------------------------------------------------------------
function pskill { 
	typeset pid
    
    # Find it
	pid=$(ps -ax | grep ${1} | grep -v grep | awk '{ print $1 }')

    # Kill it
	echo -n "killing ${1} (process ${pid})..."
	kill -9 ${pid}
	echo "slaughtered."
}

#------------------------------------------------------------------------------
# sshto
# Login to a remote host using ssh
#------------------------------------------------------------------------------
function sshto {
  typeset ids

  ids=`ssh-add -l`

  if [ "$ids" = 'The agent has no identities.' ]
  then
    ssh-add
  fi

  if [ "$RUNNING_CYGWIN" = 'true' ]
  then
    ssh -Y $1
  else
    ssh -X $1
  fi
}

#------------------------------------------------------------------------------
# setdisplay
# Function to set the DISPLAY and HOSTDISPLAY environment variables
#------------------------------------------------------------------------------
function setdisplay {
    export DISPLAY=${1}:0.0
    export HOSTDISPLAY=${DISPLAY}
}

#------------------------------------------------------------------------------
# tgz
# tars and gzips a list of files/directories
# usage: tgz archive dir1 dir2...
# creates archive.tar.gz
#------------------------------------------------------------------------------
function tgz {
	typeset tarball=${1}.tar
	typeset archive=${1}.tar.gz
	echo packing ${archive}
	shift
	tar cf ${tarball} $@
	gzip ${tarball}
}

#------------------------------------------------------------------------------
# untgz
# unpacks a list of .tgz .tar.gz or tar.Z archives
#------------------------------------------------------------------------------
function untgz {
    for archive in $@
    do
        echo unpacking ${archive}
        tar zxvf ${archive} 
	done
}

#------------------------------------------------------------------------------
# lstgz
# List the files in a set of archives
#------------------------------------------------------------------------------
function lstgz {
    for archive in $@
    do
        echo listing ${archive}
        tar ztf ${archive}
    done
}

#-----------------------------------------------------------------------------
# pushd 
# Push current directory onto a stack 
#-----------------------------------------------------------------------------
function pushd {
    typeset dirname=$1
    
    if [[ -d $dirname && -x $dirname ]]
    then
        # if dirname is not the root directory
        if [[ $dirname != / ]]
        then 
            # remove the trailing / if it exists
            dirname=${dirname%/}
        fi

        "cd" $dirname
        DIRSTACK="$dirname ${DIRSTACK:-$OLDPWD}"
        print $DIRSTACK
    else
        print still in $PWD.
    fi
}

#-----------------------------------------------------------------------------
# popd 
# Pop a directory off of the stack 
#-----------------------------------------------------------------------------
function popd {
    DIRSTACK=${DIRSTACK#* }
    "cd" ${DIRSTACK%% *}
    print $DIRSTACK
}

#-----------------------------------------------------------------------------
# _cd
# make all cd command use pushd 
#-----------------------------------------------------------------------------
function _cd {
    if [ $# -lt 1 ]
    then
        eval $(echo "pushd '${HOME}' > /dev/null")
    else
        for d in "$@"
        do
            if echo "${d}" | grep '^[^/].*' > /dev/null 2>&1
            then
                d="$PWD/${d}"
            fi
            eval $(echo "pushd '${d}' > /dev/null")
        done
    fi
}

#-----------------------------------------------------------------------------
# _dirs
# list the directories on the directory stack 
#-----------------------------------------------------------------------------
function _dirs {
    n=1

    for d in $DIRSTACK
    do
        print "$n ${d}"
        n=$(($n + 1))
    done
}

alias cd=_cd
alias dirs=_dirs
alias p=popd

#-----------------------------------------------------------------------------
# sd
# select directory from dirs
#------------------------------------------------------------------------------
function sd {
    PS3='directory (0 to stay put)? '

    select selection in $(dirs)
    do
        if [ ${selection} ]
        then
            cd ${selection}
            break
        else
            echo 'Staying put.'
            break
        fi
    done
}

#------------------------------------------------------------------------------
# lines
# Print the number of lines in file(s)
#------------------------------------------------------------------------------
function lines {
    wc -l $@
}


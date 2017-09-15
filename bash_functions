###############################################################################
#
# bash_functions - Bash initializtion file to define functions
#
###############################################################################
function emacs() {
  local prg

  if [ "${RUNNING_CYGWIN}" = 'true' -o "$RUNNING_MINGW" = 'true' ]
  then
    prg="${EMACSHOME}/bin/runemacs"
  elif [ "${RUNNING_DARWIN}" = 'true' ]
  then
    prg=/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs
  else
    prg=emacs
  fi

  #command $prg -bg 'light yellow' -cr salmon -g 80x50 $@
  command $prg -g 80x50 $@
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
function findsrc() {
  if [ "${RUNNING_DARWIN}" = 'true' ]
  then
    # BSD find
    find . -name .svn -prune -or \
           \( -type f -and \
           \( -name '*.java' -or -name '*.xml' -or -name '*.sql' \) \) -print
  else
    # GNU find
    find . -type f -a -regex '.*\.\(java\|xml\|sql\)$' -print
  fi
}

#---------------------------------------------------------------------------
# srcsearch - find source files and search them for an regex
#---------------------------------------------------------------------------
function srcsearch() {
  findsrc | xargs grep -i --color=auto ${1}
}

#---------------------------------------------------------------------------
# Find process to kill and kill it.
#---------------------------------------------------------------------------
function pskill() { 
  local pid

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
function sshto() {
  local ids

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
function setdisplay() {
    export DISPLAY=${1}:0.0
    export HOSTDISPLAY=${DISPLAY}
}

#------------------------------------------------------------------------------
# tgz
# tars and gzips a list of files/directories
# usage: tgz archive dir1 dir2...
# creates archive.tar.gz
#------------------------------------------------------------------------------
function tgz() {
    tarball=${1}.tar
    archive=${1}.tar.gz
    echo packing ${archive}
    shift
    tar cf ${tarball} $@
    gzip ${tarball}
}

#------------------------------------------------------------------------------
# untgz
# unpacks a list of .tgz .tar.gz or tar.Z archives
#------------------------------------------------------------------------------
function untgz() {
    local archive

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
function lstgz() {
    local archive

    for archive in $@
    do
        echo listing ${archive}
        tar ztf ${archive}
    done
}

#------------------------------------------------------------------------------
# cpdir
# Copy directory tree to the current directory
#
# cd /new/parent/dir
# cpdir /old/parent/dir dir_to_copy
#------------------------------------------------------------------------------
function cpdir() {
    local src_parent_dir=$1
    local src=$2
    tar -cf - -C $src_parent_dir $src | tar -xvpf -
}


#-----------------------------------------------------------------------------
# cd
# make all cd command use pushd 
#-----------------------------------------------------------------------------
function cd() {
    if [ $# -lt 1 ]
    then
        eval $(echo "pushd '${HOME}' > /dev/null")
    else
        local d

        for d in "$@"
        do
            eval $(echo "pushd '${d}' > /dev/null")
        done
    fi
}

#-----------------------------------------------------------------------------
# sd
# select directory from dirs
#------------------------------------------------------------------------------
function sd() {
    local selection
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
function lines() {
    wc -l $@
}
###############################################################################
#
# winfuncs: Win32 and DOS integration functions for bash on Gnu-Win32
#
# dos: Run a DOS command/batch file from shell, without needing .bat suffix,
# or run DOS shell if no arguments.  Note that some .com files will not
# run directly from bash - you can use 'dos foo.com' instead.
#
###############################################################################
if [ "$RUNNING_CYGWIN" = 'true' ]
then
    #--------------------------------------------------------------------------
    # dos
    # Execute a command under a MS shell.
    # This function runs faster if you have Perl for Win32's cmd32.exe.
    # If you don't have this program, change 'cmd32.exe' to 'command.com /c'
    # for Win95 and 'cmd.exe /c' for WinNT.
    #--------------------------------------------------------------------------
    function dos() {
        if [ $# -ne 0 ];          # If there are args
        then
            cmd.exe /c "$@"
        else
            cmd.exe
        fi
    }

    #--------------------------------------------------------------------------
    # open
    # Open a Windows document or application in a new window.
    # Arguments after the first can be used to provide parameters.
    # 
    # Usage: open file1 ...
    #
    # Try 'open notepad', 'open bash', 'open foo.txt', 'open command.com', 
    # 'open "long file name.txt"', etc.
    #--------------------------------------------------------------------------
    function open() {
        # Put double quotes around all arguments for use by Win32 shell
        local arg=''
        local list=''
        if test $# -eq 0; then
            echo >&2 "Usage: open file1 ..."
            return
        fi

        for arg in "$@"
        do
            list="$list \"$arg\""
        done
        set -- $list

        dos start "$@"
    }
fi

###############################################################################
# End of bash_functions
###############################################################################

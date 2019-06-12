#############################################################################
# zshrc
############################################################################

if [[ -f /etc/zsh/zprofile ]]; then
    source /etc/zsh/zprofile
fi

#---------------------------------------------------------------------------
# Set the shell process's file creation mask
#---------------------------------------------------------------------------
umask 022

#---------------------------------------------------------------------------
# Set the RUNNING_CYGWIN environment variable.
# Set the RUNNING_DARWIN environment variable.
#---------------------------------------------------------------------------
RUNNING_CYGWIN=false;
RUNNING_DARWIN=false;

case $OSTYPE in
  cygwin)
    RUNNING_CYGWIN=true;;
  darwin*)
    RUNNING_DARWIN=true;;
  linux*)
    RUNNING_LINUX=true;;
esac
export RUNNING_CYGWIN RUNNING_DARWIN

#---------------------------------------------------------------------------
# Set the PRINTER environment variable.
#---------------------------------------------------------------------------
if [[ i$RUNNING_CYGWIN == true ]]
then
    export PRINTER='\\APOLLO\Dev'
fi

#---------------------------------------------------------------------------
# Set the HOSTNAME environment variable.
#---------------------------------------------------------------------------
if [[ -z $HOSTNAME ]]
then
    export HOSTNAME=$(hostname)
fi

#---------------------------------------------------------------------------
# Set the temporary directory
#
# If /tmp exists and is a directory
#	Set TMP to /tmp
#---------------------------------------------------------------------------
if [[ -d /tmp ]]; then
    export TMP=/tmp
fi

#---------------------------------------------------------------------------
# Set PERL5%LIB
#---------------------------------------------------------------------------
typeset -T PERL5LIB perl5lib
typeset -U perl5lib # keep elements in sgml_cat_files unique
export PERL5LIB

perl5lib=(
    /usr/local/share/perl5/site_perl(N)           #  
    )

#---------------------------------------------------------------------------
# Setup for Fink on Darwin
#---------------------------------------------------------------------------
if [[ $RUNNING_DARWIN == true && -x /sw/bin/init.sh ]]
then
    . /sw/bin/init.sh
fi

#---------------------------------------------------------------------------
# Set the JAVA_HOME environment variable.
# Everything java depends on this.
#---------------------------------------------------------------------------
if [[ $RUNNING_CYGWIN == true ]]
then
  if [[ -d '/cygdrive/c/Program Files/Java/jdk1.6.0_29' ]]
  then
    export JAVA_VERSION='1.6.0_29'
    export JAVA_HOME="c:\\Program Files\\Java\\jdk${JAVA_VERSION}"
  fi
elif [[ $RUNNING_DARWIN == true ]]
then
    export JAVA_VERSION='1.6.0'
    export JAVA_HOME='/Library/Java/Home'
elif [[ -d '/usr/lib/jvm/java-6-openjdk' ]]
then
    export JAVA_VERSION='1.6.0'
    export JAVA_HOME='/usr/lib/jvm/java-6-openjdk'
fi

#------------------------------------------------------------------------------
# Set the CATALINA_HOME environment variable
#------------------------------------------------------------------------------
tomcat_version=5.5.20

if [[ -d /usr/local/apache-tomcat-${tomcat_version} ]]
then
    if [[ $RUNNING_CYGWIN == true ]]
    then
        CATALINA_HOME=$(cygpath -w /usr/local/apache-tomcat-${tomcat_version})
    else
        CATALINA_HOME="/usr/local/apache-tomcat-${tomcat_version}"
    fi
    export CATALINA_HOME
fi

#---------------------------------------------------------------------------
# Set the M2_HOME environment variable. (Maven 2)
#---------------------------------------------------------------------------
m2_version='2.0.4'

if [[ -d /usr/local/maven-${m2_version} ]]
then
    if [[ $RUNNING_CYGWIN == true ]]
    then
        M2_HOME=$(cygpath --mixed "/usr/local/maven-${m2_version}")
    else
        M2_HOME="/usr/local/maven-${m2_version}"
    fi
    export M2_HOME
fi

#---------------------------------------------------------------------------
# Set the FOP_HOME environment variable.
#---------------------------------------------------------------------------
fop_version='0.20.5'

if [[ -d /usr/local/fop-${fop_version} ]]
then
    if [[ $RUNNING_CYGWIN == true ]]
    then
        FOP_HOME=$(cygpath --mixed /usr/local/fop-${fop_version})
    else
        FOP_HOME="/usr/local/fop-${fop_version}"
    fi
    export FOP_HOME
fi

#---------------------------------------------------------------------------
# Set the JETTY_HOME environment variable.
#---------------------------------------------------------------------------
jetty_version='5.1.10'
if [[ -d /usr/local/jetty-${jetty_version} ]]
then
    if [[ $RUNNING_CYGWIN == true ]]
    then
        JETTY_HOME=$(cygpath --mixed /usr/local/jetty-${jetty_version})
    else
        JETTY_HOME="/usr/local/jetty-${jetty_version}"
    fi
    export JETTY_HOME	
fi

#---------------------------------------------------------------------------
# Set the GROOVY_HOME environment variable.
#---------------------------------------------------------------------------
GROOVY_VERSION='1.0-beta-10'

if [[ -d /usr/local/groovy-$GROOVY_VERSION ]]
then
    if [[ $RUNNING_CYGWIN == true ]]
    then
        GROOVY_HOME=$(cygpath --mixed /usr/local/groovy-$GROOVY_VERSION)
    else
        GROOVY_HOME="/usr/local/groovy-$GROOVY_VERSION"
    fi
    export GROOVY_HOME
elif [[ -d /opt/local/share/java/groovy ]]
then
  GROOVY_HOME='/opt/local/share/java/groovy'
  export GROOVY_HOME
fi

#---------------------------------------------------------------------------
# Set the GANT_HOME environment variable.
#---------------------------------------------------------------------------
if [[ -d /opt/local/share/java/gant ]]
then
  GANT_HOME='/opt/local/share/java/gant'
  export GANT_HOME
fi

#------------------------------------------------------------------------------
# Set the ORACLE_HOME environment variable
#------------------------------------------------------------------------------
if [[ -z $ORACLE_HOME ]]
then
    oracle_version=92
    if [[ $RUNNING_CYGWIN == true ]]
    then
        if [[ -d "/cygdrive/c/oracle/ora$oracle_version" ]]
        then
            export ORACLE_HOME="c:\\oracle\\ora$oracle_version"
        fi
    elif [[ -d /usr/local/instantclient_11_2 ]]
    then
        export ORACLE_HOME='/usr/local/instantclient_11_2'
    fi
fi

if [[ -z SQL_PATH ]]
then
    if [[ $RUNNING_CYGWIN == true ]]
    then
        if [[ -d '/cygdrive/c/instantclient_11_2' ]]
        then
            export SQL_PATH='c:\\instantclient_11_2'
        fi
    fi
fi

if [[ -e ${HOME}/config/ora_config ]]
then
  . ${HOME}/config/ora_config
fi
    
#---------------------------------------------------------------------------
# Set the PATH environment variable.
# Colon separated list of directories in which the shell looks for commands
#---------------------------------------------------------------------------

#
# Initialize variables
#
USER=${USER:-$(logname)}
unset PATH

if [[ $USER == root ]]
then
    path=(
        /usr/local/sbin(N)
        /usr/local/bin(N)
        /usr/sbin(N)
        /usr/bin(N)
        /sbin(N)
        /bin(N)
        /usr/bin/X11(N)
        /usr/X11R6/bin(N)
        ~/bin(N)
        /usr/games(N)
    )

elif [[ $RUNNING_DARWIN == true ]]
then
    path=(
        ~/bin(N)
        /usr/local/sbin(N)
        /usr/local/bin(N)
        /usr/local/TeX/bin/powerpc-darwin6.8(N)
        /opt/local/sbin(N)
        /opt/local/bin(N)
        /sw/sbin(N)
        /sw/bin(N)
        /usr/sbin(N)
        /usr/bin(N)
        /sbin(N)
        /bin(N)
        /usr/X11R6/bin(N)
        /Developer/Tools(N)
        /usr/share/java/Tools(N)
        /usr/local/mysql/bin(N)
        .
    )
else
    path=(
        ~/bin(N)
        /usr/local/bin(N)
        /usr/bin(N)
        /bin(N)
        /usr/bin/X11(N)
        /usr/X11R6/bin(N)
	/usr/local/TeX/bin/win32(N)  # Win32 TexLive
        /usr/games(N)
        .
    )
fi

#
# If JAVA_HOME is set add the java bin dir to the path
#
if [[ -n $JAVA_HOME ]]
then
    if [[ $RUNNING_CYGWIN == true ]]
    then
        path=("$(cygpath -u $JAVA_HOME/bin)" $path)
    else
        path=($JAVA_HOME/bin $path )
    fi
fi

#
# if ANT_HOME/bin exists add it
#
ant_version='1.8.2'

if [[ -d /usr/local/apache-ant-${ant_version}/bin ]]
then
    path=($path /usr/local/apache-ant-${ant_version}/bin)
fi

#
# if M2_HOME/bin exists add it
#
if [[ -n $M2_HOME && -d $M2_HOME/bin ]]
then
  if [[ $RUNNING_CYGWIN == true ]]
  then
    path=($path $(cygpath -u $M2_HOME/bin))
  else
    path=($path $M2_HOME/bin)
  fi
fi

#
# if FOP_HOME exists add it
#
if [[ -n $FOP_HOME && -d $FOP_HOME ]]
then
  if [[ $RUNNING_CYGWIN == true ]]
  then
    path=($path $(cygpath -u $FOP_HOME))
  else
    path=($path $FOP_HOME)
  fi
fi

#
# if GROOVY_HOME/bin exists add it
#
if [[ -n $GROOVY_HOME && -d $GROOVY_HOME/bin ]]
then
  if [[ $RUNNING_CYGWIN == true ]]
  then
    path=($path $(cygpath -u $GROOVY_HOME/bin))
  else
    path=($path $GROOVY_HOME/bin)
  fi
fi

#
# if ORACLE_HOME/bin exists add it
#
if [[ -n $ORACLE_HOME && -d $ORACLE_HOME/bin ]]
then
  if [[ $RUNNING_CYGWIN == true ]]
  then
    path=($path $(cygpath -u $ORACLE_HOME/bin))
  else
    path=($path $ORACLE_HOME/bin)
  fi
fi

#
# if SQL_PATH exists add it
#
if [[ -n $SQL_PATH  ]]
then
  if [[ $RUNNING_CYGWIN == true ]]
  then
    if [[ -d $(cygpath -u $SQL_PATH) ]]
    then
      path=($path $(cygpath -u $SQL_PATH))
    fi
  else
    if [[ -d $SQL_PATH ]]
    then 
      path=($path $SQL_PATH)
    fi
  fi
fi

#
# if running under cygwin add windows system 
#
if [[ $RUNNING_CYGWIN == true ]]
then
    PATH=$PATH${PATH+:}$(cygpath -u $WINDIR)

    if [[ -d $(cygpath -u $WINDIR/system) ]]
    then
        path=($path $(cygpath -u $WINDIR/system))
    fi

    if [[ -d $(cygpath -u $WINDIR/system32) ]]
    then
        path=($path $(cygpath -u $WINDIR/system32))
    fi

    if [[ -d $(cygpath -u $WINDIR/system32/Wbem) ]]
    then
        path=($path $(cygpath -u $WINDIR/system32/Wbem))
    fi

#    path=($path '/cygdrive/c/Program Files/Borland/Starteam SDK 6.0/Lib')
#    path=($path '/cygdrive/c/Program Files/Borland/Starteam SDK 6.0/bin')
fi

#------------------------------------------------------------------------------
# Readline configuration
#------------------------------------------------------------------------------
if [[ -f $HOME/.inputrc ]]
then
    INPUTRC="$HOME/.inputrc"
elif [[ -f /etc/inputrc ]]
then
    INPUTRC=/etc/inputrc
fi

#------------------------------------------------------------------------------
# Terminal definition
#------------------------------------------------------------------------------
if [[ -z $TERM ]]
then
    TERM=linux
fi
export TERM INPUTRC

#---------------------------------------------------------------------------
# Setup the LS_COLORS environment variable
#---------------------------------------------------------------------------
if [[ -x /usr/bin/dircolors && -f ~/config/dircolors ]]
then
    eval $(dircolors --bourne-shell ~/config/dircolors)
fi

# a     black 
# b     red 
# c     green 
# d     brown 
# e     blue 
# f     magenta 
# g     cyan 
# h     light grey 
# A     bold black, usually shows up as dark grey 
# B     bold red 
# C     bold green 
# D     bold brown, usually shows up as yellow 
# E     bold blue 
# F     bold magenta 
# G     bold cyan 
# H     bold light grey; looks like bright white 
# x     default foreground or background 

#   1. directory
#   2. symbolic link
#   3. socket
#   4. pipe
#   5. executable
#   6. block special
#   7. character special
#   8. executable with setuid bit set
#   9. executable with setgid bit set
#  10. directory writable to others, with sticky bit
#  11. directory writable to others, without sticky bit

# dir = bold blue
# sym link = bold magenta
# socket   = bold green
# pipe     = yellow
# exe      = bold red
# block sp = blue on cyan
# char sp  = blue on brown
# setuid x = black on red
# setgid x = black on cyan
# dir o+ws = black on green
# dir o+w  = black on brown

export CLICOLOR=ON
export LSCOLORS=ExFxCxDxBxegedabagacad

#---------------------------------------------------------------------------
# Set LOCATE_PATH - A colon separated list of database file names by the
# locate command.
#---------------------------------------------------------------------------
if [[ $RUNNING_CYGWIN == true ]]
then
    export LOCATE_PATH=/usr/var/locatedb
fi

#---------------------------------------------------------------------------
# Set CDPATH - A colon separated list of directories used as a
# search path for 'cd'
#---------------------------------------------------------------------------
cdpath=(
    .
    ~
    /cygdrive/c/dev/environments(N)
    /cygdrive/c/projects(N)
    /cygdrive/c/SESCO(N)
    )

#---------------------------------------------------------------------------
# Set CPATH - A colon separated list of directories searched
# for include files.
#---------------------------------------------------------------------------
CPATH='/usr/include'

if [[ -d /opt/local/include ]]
then
    CPATH="/opt/local/include:${CPATH}"
fi

if [[ -d /usr/local/include ]]
then
    CPATH="/usr/local/include:${CPATH}"
fi
export CPATH

#---------------------------------------------------------------------------
# Set LIBRARY_PATH - A colon separated list of directories searched
# for libraries.
#---------------------------------------------------------------------------
LIBRARY_PATH='/usr/lib'

if [[ -d /opt/local/lib ]]
then
    LIBRARY_PATH="/opt/local/lib:${LIBRARY_PATH}"
fi

if [[ -d /usr/local/lib ]]
then
    LIBRARY_PATH="/usr/local/lib:${LIBRARY_PATH}"
fi
export LIBRARY_PATH

#---------------------------------------------------------------------------
# Set LD_LIBRARY_PATH - A colon separated list of directories searched
# for dynamic libraries.
#---------------------------------------------------------------------------
if [[ "$RUNNING_LINUX" == 'true' || "$RUNNING_CYGWIN" == 'true' ]]
then
#  LD_LIBRARY_PATH='/usr/lib'
#
  if [[ -d /usr/local/lib ]]
  then
    LD_LIBRARY_PATH="/usr/local/lib:${LD_LIBRARY_PATH}"
    export LD_LIBRARY_PATH
  fi
fi

#---------------------------------------------------------------------------
# Set DYLD_LIBRARY_PATH - A colon separated list of directories searched
# for dynamic libraries.
#
# This was a bad idea! When MacPorts has a different version of a library it
# causes link errors for things compiled against the system libs.
#---------------------------------------------------------------------------
#if [[ "$RUNNING_DARWIN" = 'true' ]]
#then
#  DYLD_LIBRARY_PATH='/usr/lib'
#
#  if [[ -d /opt/local/lib ]]
#  then
#    DYLD_LIBRARY_PATH="/opt/local/lib:${DYLD_LIBRARY_PATH}"
#  fi
#
#  if [[ -d /usr/local/lib ]]
#  then
#    DYLD_LIBRARY_PATH="/usr/local/lib:${DYLD_LIBRARY_PATH}"
#  fi
#  export DYLD_LIBRARY_PATH
#fi

#---------------------------------------------------------------------------
# Set PKG_CONFIG_PATH - A colon separated list of directories searched
# for pkg-config config files.
#---------------------------------------------------------------------------
if [[ -d /Library/Frameworks/Mono.framework/Versions/Current/lib/pkgconfig ]]
then
  PKG_CONFIG_PATH="${PKG_CONFIG_PATH}:/Library/Frameworks/Mono.framework/Versions/Current/lib/pkgconfig"
fi
export PKG_CONFIG_PATH

#---------------------------------------------------------------------------
#
# Set the default pager
#---------------------------------------------------------------------------
# This was unreliable:
#if [[ -x /usr/bin/aless ]]
#then
#    PAGER=aless
#elif [[ -x /usr/bin/less ]]

if [[ -x /usr/bin/less ]]
then
    PAGER=less

 
    ##########################################################################
    #
    # Set environment variables for less
    #
    ##########################################################################
 
    #-------------------------------------------------------------------------
    # Set the options for less
    #
    #	-C : Scroll by clearing and repainting
    #	-e : Exit at end of file (second time)
    #	-g : Highlight only the string found by last search
    #	-i : Ignore case in searches
    #	-n : Suppress line numbers	
    #	-m : Prompt style (Like more)
    #	-Q : Quiet, do not ring the bell for errors
    #	-s : Squeeze multiple blank lines into one
    #	-x : Set tab stops
    #-------------------------------------------------------------------------
    LESS='-eisnmQRs -x4'
    export LESS
else
    PAGER=more
fi
export PAGER

#---------------------------------------------------------------------------
# Set the default editor
#---------------------------------------------------------------------------
EDITOR=vim
VISUAL=vim

export EDITOR VISUAL

#---------------------------------------------------------------------------
# Set the location of the artistic style option file
#---------------------------------------------------------------------------
if [ -f "${HOME}/config/astylerc" ]
then
    export ARTISTIC_STYLE_OPTIONS="${HOME}/config/astylerc"
fi

#---------------------------------------------------------------------------
# Set the cvs root
#---------------------------------------------------------------------------
#if [[ $HOST == elrond.leebrinton.net ]]
#then
#    export CVSROOT='/usr/local/cvsroot'
#else
#    export CVS_RSH='ssh'
#    export CVSROOT=':ext:lee@elrond:/usr/local/cvsroot'
#fi

#---------------------------------------------------------------------------
# Set XNLSPATH - X Locales
#---------------------------------------------------------------------------
if [[ -d /usr/X11R6/lib/X11/locale ]]
then
    export XNLSPATH=/usr/X11R6/lib/X11/locale
elif [[ -d /usr/X11R6/lib/X11/nls ]]
    then
    export XNLSPATH=/usr/X11R6/lib/X11/nls
fi

#---------------------------------------------------------------------------
# Set the sgml catalog and xml catalog
#---------------------------------------------------------------------------
typeset -T SGML_CATALOG_FILES sgml_cat_files
typeset -U sgml_cat_files # keep elements in sgml_cat_files unique
export SGML_CATALOG_FILES

sgml_cat_files=(
  /usr/local/share/sgml/catalog(N)                     # additional sgml on cygwin
  /opt/local/share/sgml/iso8879/catalog(N)             # macports iso8879 entities
  /sw/share/sgml/entities/iso8879/catalog(N)           # fink iso8879 entities
  /sw/share/sgml/openjade-1.3/dsssl/catalog(N)         # openjade on fink
  /sw/share/dgml/dsssl/docbook-dsssl-nwalsh/catalog(N) # docbook dsssl on fink
  /usr/share/openjade/catalog(N)                       # openjade on cygwin
  /usr/share/xml/docbook/4.3/docbook.cat(N)            # docbook xml on cygwin
  /usr/local/share/docbook-dsssl/catalog(N)            # docbook dsssl on cygwin
  /usr/share/OpenSP/*.soc(N)                           # opensp catalogs on cygwin
  /opt/local/share/OpenSP/*.soc(N)                     # opensp catalogs on macports
  /sw/share/OpenSP/*.soc(N)                            # opensp catalogs on fink
  /etc/sgml/catalog(N)                                 # traditional master catalog
  /sw/share/sgml/catalog(N)                            # fink sgml catalog
)

typeset -T XML_CATALOG_FILES xml_cat_files ' '
typeset -U xml_cat_files # keep elements in xml_cat_files unique
export XML_CATALOG_FILES

xml_cat_files=(
  /etc/xml/catalog(N)
  /opt/local/etc/xml/catalog(N)
  /sw/etc/xml/catalog(N)
  /usr/local/lib/xml/catalog(N)
)

#------------------------------------------------------------------------------
# Set up html tidy
#------------------------------------------------------------------------------
export HTML_TIDY="$HOME/config/tidy"

#---------------------------------------------------------------------------
# Set the EMACSHOME environment variable.
#---------------------------------------------------------------------------
if [[ $RUNNING_CYGWIN == true ]]
then
  all_emacsen=(
    23.1
    23.3
    24.1
    24.2
    24.3
)

  for emacs_version in $all_emacsen
  do
    if [[ -d /usr/local/emacs-${emacs_version} ]]
    then
      EMACSHOME=/usr/local/emacs-${emacs_version}
    fi
  done

  if [[ -n $EMACSHOME ]]
  then
    export EMACSHOME
  fi
fi

#------------------------------------------------------------------------------
# Set the MANPATH environment variable.
# A colon separated list of directories in which to search for man files
#------------------------------------------------------------------------------
#if [[ -d /usr/local/ast/man ]]
#then
#    export MANPATH="${MANPATH}:/usr/local/ast/man"
#fi

#if [[ -d /sw/share/man ]]
#then
#    echo 'Adding /sw/share/man'
#    export MANPATH="/sw/share/man:${MANPATH}"
#fi

#if [[ -d /sw/lib/perl5/5.8.8/man ]]
#then
#    echo '     Adding /sw/lib/perl5/5.8.8/man'
#    export MANPATH="${MANPATH}:/sw/lib/perl5/5.8.8/man"
#fi

#if [[ -d /usr/X11R6/man ]]
#then
#        echo 'Adding /usr/X11R6/man'
#    export MANPATH="${MANPATH}:/usr/X11R6/man"
#fi

#if [[ -d /opt/local/share/man ]]
#then
#    echo 'Adding /opt/local/share/man'
#    export MANPATH="${MANPATH}:/opt/local/share/man"
#fi

#typeset -T MANPATH mpath # Tie the array manpath to MANPATH
#typeset -U mpath           # keep elements in manpath unique
#export MANPATH

manpath=(
          /usr/local/ast/man(N)
          /sw/share/man(N)
          /sw/lib/perl5/5.8.8/man(N)
          /usr/X11R6/man(N)
          /opt/local/share/man(N)
          /usr/local/mysql/man(N)
          ${manpath}
         )

#---------------------------------------------------------------------------
# Set the INFOPATH environment variable.
# A colon separated list of directories in which to search
# for info 'dir' files
#---------------------------------------------------------------------------
typeset -T INFOPATH infopath # Tie the array infopath to INFOPATH
typeset -U infopath          # keep elements in infopath unique
export INFOPATH

infopath=(
          /usr/autotool/devel/share/info(N)
          /sw/share/info(N)
          /sw/info(N)
          /usr/share/info(N)
          /usr/info(N)
          /opt/local/share/info(N)
          /usr/local/share/info(N)
          $EMACSHOME/info(N)
          /usr/autotool/stable/share/info(N)
          $infopath
         )

#---------------------------------------------------------------------------
# Set the SYSSCREENRC variable location of the system level screen
# config file.
#---------------------------------------------------------------------------
if [[ $RUNNING_CYGWIN == true ]]
then
    export SYSSCREENRC=/etc/screenrc
fi

#---------------------------------------------------------------------------
# Set the SCHEME_LIBRARY_PATH variable location of slib.
# Set the SCM_INIT_PATH       variable location of scm's init file.
#---------------------------------------------------------------------------
if [[ -d /usr/share/guile/1.4/slib ]]
then
    export SCHEME_LIBRARY_PATH=/usr/share/guile/1.4/slib
elif [[ -d /usr/local/lib/slib ]]
then
    export SCHEME_LIBRARY_PATH=/usr/local/lib/slib/
fi

if [[ -f /usr/local/lib/scm/Init5d4.scm ]]
then
    export SCM_INIT_PATH=/usr/local/lib/scm/Init5d4.scm
fi

#---------------------------------------------------------------------------
# Build a new signature file
#---------------------------------------------------------------------------
if [[ -x /usr/bin/signify ]]
then
    signify >! ~/.signature
fi

#---------------------------------------------------------------------------
# Display todays daily verse from the King James Bible
#---------------------------------------------------------------------------
if [[ -x /usr/bin/verse ]]
then
    echo
    verse
    echo
fi

#---------------------------------------------------------------------------
# Start in the users home directory
#---------------------------------------------------------------------------
if [[ $HOME != $PWD ]]
then
    cd $HOME
fi

#---------------------------------------------------------------------------
# Setup ssh-agent
#---------------------------------------------------------------------------
if [[ $RUNNING_CYGWIN == true ]]
then
    if [[ -x ~/bin/sshtool ]]
    then
        eval $(~/bin/sshtool)
    fi

    echo 'Loading ssh keys ...'
    
    for key in id_rsa id_dsa trexone_rsa
    do
        if [[ -e ~/.ssh/${key} ]]
        then
            echo "$key"
            keychain "${HOME}/.ssh/${key}"
            source "${HOME}/.keychain/$(hostname)-sh" > /dev/null
        fi
    done 
fi

#---------------------------------------------------------------------------
# Set ZSH options
#---------------------------------------------------------------------------
setopt NO_all_export

# If unset, key functions that list completions try to return to the
# last prompt if given a numeric argument. If set these functions
# try to return to the last prompt if given _no_ numeric argument.   
setopt always_last_prompt

setopt NO_always_to_end

# If this is set, zsh sessions will append their history list to the
# history file, rather than overwrite it. Thus, multiple parallel
# zsh sessions will all have their history lists added to the
# history file, in the order they are killed.
setopt append_history

# If a command is issued that can't be executed as a normal command,
# and the command is the name of a directory, perform the cd command
# to that directory.
setopt autocd

# Automatically list choices on an ambiguous completion.
setopt auto_list

# Automatically use menu completion after the second consecutive
# request for completion, for example by pressing the tab key
# repeatedly. This option is overridden by MENU_COMPLETE.
setopt auto_menu

setopt NO_auto_name_dirs

# If a parameter name was completed and a following character
# (normally a space) automatically inserted, and the next character
# typed is one of those that have to come directly after the name
# (like `}', `:', etc.), the automatically added character is
# deleted, so that the character typed comes immediately after the
# parameter name.  Completion in a brace expansion is affected
# similarly: the added character is a `,', which will be removed if
# `}' is typed next.
setopt auto_param_keys

# If a parameter is completed whose content is the name of a
# directory, then add a trailing slash instead of a space.
setopt auto_param_slash

# Make cd push the old directory onto the directory stack.
setopt autopushd

# When the last character resulting from a completion is a slash and
# the next character typed is a word delimiter, a slash, or a
# character that ends a command (such as a semicolon or an
# ampersand), remove the slash.
setopt auto_remove_slash

setopt NO_auto_resume

# If a pattern for filename generation is badly formed, print an
# error message.  (If this option is unset, the pattern will be left
# unchanged.)
setopt bad_pattern

# Perform textual history expansion, `csh'-style, treating the
# character `!' specially.
setopt bang_hist

setopt NO_beep

# Expand expressions in braces which would not otherwise undergo
# brace expansion to a lexically ordered list of all the characters.
# See *Note Brace Expansion::.
setopt brace_ccl

setopt NO_bsd_echo

# Output hexadecimal numbers in the standard C format, for example
# 0xFF' instead of the usual `16#FF'.  If the option OCTAL_ZEROES
# is also set (it is not by default), octal numbers will be treated
# similarly and hence appear as `077' instead of `8#77'.  This
# option has no effect on the choice of the output base, nor on the
# output of bases other than hexadecimal and octal.  Note that these
# formats will be understood on input irrespective of the setting of
# C_BASES.
setopt cbases

# If the argument to a cd command (or an implied cd with the AUTO_CD
# option set) is not a directory, and does not begin with a slash,
# try to expand the expression as if it were preceded by a `~'.
setopt cdable_vars

setopt NO_chase_links
setopt NO_clobber

# Prevents aliases on the command line from being internally
# substituted before completion is attempted.  The effect is to make
# the alias a distinct command for completion purposes.
setopt complete_aliases

# If unset, the cursor is set to the end of the word if completion is
# started. Otherwise it stays there and completion is done from both
# ends.
setopt complete_in_word

# Try to correct the spelling of commands.
setopt correct

# Try to correct the spelling of all arguments in a line.
setopt correctall

setopt NO_csh_junkie_history
setopt NO_csh_junkie_loops
setopt NO_csh_junkie_quotes
setopt NO_csh_null_glob

# Perform = filename expansion.  (See *Note Filename Expansion::.)
setopt equals

# Treat the `#', `~' and `^' characters as part of patterns for
# filename generation, etc.  (An initial unquoted `~' always
# produces named directory expansion.)
setopt extended_glob

# Save each command's beginning timestamp (in seconds since the
# epoch) and the duration (in seconds) to the history file.  The
# format of this prefixed data is:
#     `:<BEGINNING TIME>:<ELAPSED SECONDS>:<COMMAND>'.
setopt extended_history

# Don't stop flow on C-s
setopt no_flow_control

# When executing a shell function or sourcing a script, set $0
# temporarily to the name of the function/script.
setopt function_argzero

# Perform filename generation (globbing).  (See *Note Filename
# Generation::.)
setopt glob

# NO_glob_assign          \
# When the current word has a glob pattern, do not insert all the
# words resulting from the expansion but generate matches as for
# completion and cycle through them like MENU_COMPLETE. The matches
# are generated as if a `*' was added to the end of the word, or
# inserted at the cursor when COMPLETE_IN_WORD is set.  This
# actually uses pattern matching, not globbing, so it works not only
# for files but for any completion, such as options, user names, etc.
setopt glob_complete

setopt NO_glob_dots

# Treat any characters resulting from parameter expansion as being
# eligible for file expansion and filename generation, and any
# characters resulting from command substitution as being eligible
# for filename generation.  Braces (and commas in between) do not
# become eligible for expansion.
setopt glob_subst

# Note the location of each command the first time it is executed.
# Subsequent invocations of the same command will use the saved
# location, avoiding a path search.  If this option is unset, no
# path hashing is done at all.  However, when CORRECT is set,
# commands whose names do not appear in the functions or aliases
# hash tables are hashed in order to avoid reporting them as
# spelling errors.
setopt hash_cmds

# Whenever a command name is hashed, hash the directory containing
# it, as well as all directories that occur earlier in the path.
# Has no effect if neither HASH_CMDS nor CORRECT is set.
setopt hash_dirs

# Whenever a command completion is attempted, make sure the entire
# command path is hashed first.  This makes the first completion
# slower.
setopt hash_list_all

# Add `|' to output redirections in the history.  This allows history
# references to clobber files even when CLOBBER is unset.
setopt hist_allow_clobber

# Beep when an attempt is made to access a history entry which isn't
# there.
setopt hist_beep

# If the internal history needs to be trimmed to add the current
# command line, setting this option will cause the oldest history
# event that has a duplicate to be lost before losing a unique event
# from the list.  You should be sure to set the value of HISTSIZE to
# a larger number than SAVEHIST in order to give you some room for
# the duplicated events, otherwise this option will behave just like
# HIST_IGNORE_ALL_DUPS once the history fills up with unique events.
setopt hist_expire_dups_first

# If a new command line being added to the history list duplicates an
# older one, the older command is removed from the list (even if it
# is not the previous event).
setopt hist_ignore_all_dups

# If a new command line being added to the history list duplicates an
# older one, the older command is removed from the list (even if it
# is not the previous event).
setopt hist_ignore_dups
setopt hist_ignore_all_dups

# Remove command lines from the history list when the first
# character on the line is a space, or when one of the expanded
# aliases contains a leading space.  Note that the command lingers
# in the internal history until the next command is entered before
# it vanishes, allowing you to briefly reuse or edit the line.  If
# you want to make it vanish right away without entering another
# command, type a space and press return.
setopt hist_ignore_space

setopt NO_hist_no_functions
setopt NO_hist_no_store

# Remove superfluous blanks from each command line being added to
# the history list.
setopt hist_reduce_blanks

setopt hist_save_no_dups

# Whenever the user enters a line with history expansion, don't
# execute the line directly; instead, perform history expansion and
# reload the line into the editing buffer.
setopt hist_verify

setopt NO_hup
setopt NO_ignore_braces
setopt NO_ignore_eof

# This options works like APPEND_HISTORY except that new history
# lines are added to the $HISTFILE incrementally (as soon as they are
# entered), rather than waiting until the shell is killed.  The file
# is periodically trimmed to the number of lines specified by
# $SAVEHIST, but can exceed this value between trimmings.
setopt inc_append_history

# Allow comments even in interactive shells.
setopt interactive_comments
setopt NO_list_ambiguous
setopt NO_list_beep

# Try to make the completion list smaller (occupying less lines) by
# printing the matches in columns with different widths.
setopt list_packed

# When listing files that are possible completions, show the type of
# each file with a trailing identifying mark.
setopt list_types

# List jobs in the long format by default.
setopt long_list_jobs

# All unquoted arguments of the form `ANYTHING=EXPRESSION' appearing
# after the command name have filename expansion (that is, where
# EXPRESSION has a leading `~' or `=') performed on EXPRESSION as if
# it were a parameter assignment.  The argument is not otherwise
# treated specially; it is passed to the command as a single
# argument, and not used as an actual parameter assignment.  For
# example, in echo foo=~/bar:~/rod, both occurrences of ~ would be
# replaced.  Note that this happens anyway with typeset and similar
# statements.
setopt magic_equal_subst

setopt NO_mail_warning
setopt NO_mark_dirs
setopt NO_menu_complete

# Perform implicit `tee's or `cat's when multiple redirections are
# attempted (see *Note Redirection::).
setopt multios

# If a pattern for filename generation has no matches, print an
# error, instead of leaving it unchanged in the argument list.  This
# also applies to file expansion of an initial `~' or `='.
setopt nomatch

# Report the status of background jobs immediately, rather than
# waiting until just before printing a prompt.
setopt notify

setopt NO_null_glob

# If numeric filenames are matched by a filename generation pattern,
# sort the filenames numerically rather than lexicographically.
setopt numeric_glob_sort

setopt NO_overstrike

# Perform a path search even on command names with slashes in them.
# Thus if `/usr/local/bin' is in the user's path, and he or she types
# `X11/xinit', the command `/usr/local/bin/X11/xinit' will be
# executed (assuming it exists).  Commands explicitly beginning with
# `/', `./' or `../' are not subject to the path search.  This also
# applies to the . builtin.
# Note that subdirectories of the current directory are always
# searched for executables specified in this form.  This takes place
# before any search indicated by this option, and regardless of
# whether `.' or the current directory appear in the command search
# path.
setopt path_dirs

# When this option is set the command builtin can be used to execute
# shell builtin commands.  Parameter assignments specified before
# shell functions and special builtins are kept after the command
# completes unless the special builtin is prefixed with the command
# builtin.  Special builtins are ., :, break, continue, declare,
# eval, exit, export, integer, local, readonly, return, set, shift,
# source, times, trap and unset.
setopt posix_builtins

setopt NO_print_exit_value
setopt NO_prompt_cr

# If set, _parameter expansion_, _command substitution_ and
# _arithmetic expansion_ are performed in prompts.
setopt prompt_subst

# Don't push multiple copies of the same directory onto the
# directory stack.
setopt pushd_ignore_dups

setopt NO_pushd_minus

# Do not print the directory stack after pushd or popd.
setopt pushd_silent

# Have pushd with no arguments act like `pushd $HOME'.
setopt pushd_to_home

# Array expansions of the form `FOO${XX}BAR', where the parameter XX
# is set to (A B C), are substituted with `FOOABAR FOOBBAR FOOCBAR'
# instead of the default `FOOA B CBAR'.
setopt rc_expand_param

setopt NO_rc_quotes
setopt NO_rm_star_silent
setopt NO_rm_star_wait
setopt NO_sh_file_expansion

# If this option is set the shell tries to interpret single letter
# options (which are used with set and setopt) like `ksh' does.
# This also affects the value of the - special parameter.
setopt sh_option_letters

# Allow the short forms of for, select, if, and function constructs.
setopt short_loops

setopt NO_sh_word_split
setopt NO_single_line_zle
setopt NO_sun_keyboard_hack

# Treat unset parameters as if they were empty when substituting.
# Otherwise they are treated as an error.
setopt unset

setopt NO_verbose
 
# Use the zsh line editor.  Set by default in interactive shells
# connected to a terminal.
setopt zle

#---------------------------------------------------------------------------
# Autoload all shell functions from all directories in $fpath that
# have the executable bit on (the executable bit is not necessary, but
# gives you an easy way to stop the autoloading of a particular shell
# function).
#---------------------------------------------------------------------------
for dirname in $fpath; do
    fns=( $dirname/*~*~(N.x:t) )
    (( $#fns )) && autoload "$fns[@]"
done

#---------------------------------------------------------------------------
# Setup keys
#---------------------------------------------------------------------------
bindkey -e ## emacs key bindings

bindkey "^[[1~" beginning-of-line    ## Home
bindkey "^[[2~" overwrite-mode       ## Insert
bindkey "^[[3~" delete-char          ## Delete
bindkey "^[[4~" end-of-line          ## End
bindkey "^[[5~" up-line-or-history   ## PageUp
bindkey "^[[6~" down-line-or-history ## PageDown
bindkey "^[[A" up-line-or-search   ## up arrow for back-history-search
bindkey "^[[B" down-line-or-search ## down arrow for fwd-history-search
bindkey "^[[H"  beginning-of-line  ## Home key
bindkey "^[[8~" end-of-line        ## End key
bindkey "^[[F"  end-of-line        ## End key

bindkey "^[e" expand-cmd-path ## Esc-e for expanding path of typed command
bindkey "\ee" edit-command-line
bindkey "\Me" edit-command-line

bindkey "\er" history-beginning-search-backward ## Esc-r
bindkey "\Mr" history-beginning-search-backward ## Meta-r
bindkey "^[f" history-beginning-search-forward  ## Esc-f
bindkey "\Mf" history-beginning-search-forward  ## Meta-f
bindkey "\C-w" kill-region                      ## Ctrl-w
bindkey "\Mw" copy-region-as-kill               ## Meta-w
bindkey " " magic-space ## do history expansion on space

#---------------------------------------------------------------------------
# Turn off slow modem stuff
#---------------------------------------------------------------------------
export BAUD=0

#---------------------------------------------------------------------------
# The maximum size of the directory stack
#---------------------------------------------------------------------------
export DIRSTACKSIZE=25

#---------------------------------------------------------------------------
# Set the editor for fc
#---------------------------------------------------------------------------
export FCEDIT=vim

#---------------------------------------------------------------------------
# Suffixes to ignore when performing filename completion
#---------------------------------------------------------------------------
fignore='.o:.class:.$$$'

#---------------------------------------------------------------------------
# Setup zsh function path
#---------------------------------------------------------------------------
fpath=(
       $zdotdir/{.[z]sh/*.zwc,{.[z]sh,[l]ib/zsh}/{functions,scripts}}(N) 
       $fpath
      )
#---------------------------------------------------------------------------
# Setup history
#---------------------------------------------------------------------------
HISTFILE=~/.zshhistory
HISTSIZE=3000
SAVEHIST=3000

#---------------------------------------------------------------------------
# Maximum size of completion listing
# Only ask if line would scroll off screen
#---------------------------------------------------------------------------
LISTMAX=0

#---------------------------------------------------------------------------
# Don't ask me 'do you wish to see all XX possibilities' before
#  menu selection
#---------------------------------------------------------------------------
LISTPROMPT=''

#---------------------------------------------------------------------------
# Watching for other users
#---------------------------------------------------------------------------
LOGCHECK=60
WATCHFMT="[%B%t%b] %B%n%b has %a %B%l%b from %B%M%b"

#---------------------------------------------------------------------------
# Mail
# Set mailpath - An array of files which
# the shell periodically checks for new mail.
#---------------------------------------------------------------------------
MAILCHECK=600
mailpath=/var/spool/mail/$USER(N)

#---------------------------------------------------------------------------
# Set PS1 - The primary prompt string.
# Set PS2 - The secondary prompt string to > followed by a space. 
#---------------------------------------------------------------------------
autoload -U promptinit
promptinit

#---------------------------------------------------------------------------
# The command name to assume if a single input redirection is specified with
# no command.
#---------------------------------------------------------------------------
export READNULLCMD=$PAGER

#---------------------------------------------------------------------------
# Commands whose execution times are greater than this have timing stats
# printed
#---------------------------------------------------------------------------
REPORTTIME=15

#---------------------------------------------------------------------------
# SPROMPT - the spelling prompt
#---------------------------------------------------------------------------
SPROMPT='zsh: correct '%R' to '%r' ? ([Y]es/[N]o/[E]dit/[A]bort) '

#TMOUT=1800
#TRAPALRM () {
#  clear
#  echo Inactivity timeout on $TTY
#  echo
#  vlock -c
#  echo
#  echo Terminal unlocked. [ Press Enter ]

#---------------------------------------------------------------------------
# Choose word delimiter characters in line editor
#---------------------------------------------------------------------------
WORDCHARS=''

#---------------------------------------------------------------------------
# Completions
#---------------------------------------------------------------------------
autoload -U compinit
compinit -C # don't perform security check

# General completion technique
#zstyle ':completion:*' completer _complete _correct _approximate _prefix
zstyle ':completion:*' completer _complete _prefix
zstyle ':completion::prefix-1:*' completer _complete
zstyle ':completion:incremental:*' completer _complete _correct
zstyle ':completion:predict:*' completer _complete

# Completion caching
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/$HOST

# Expand partial paths
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-slashes 'yes'

# Include non-hidden directories in globbed file completions
# for certain commands
#zstyle ':completion::complete:*' \
#  tag-order 'globbed-files directories' all-files 
#zstyle ':completion::complete:*:tar:directories' file-patterns '*~.*(-/)'

# Don't complete backup files as executables
zstyle ':completion:*:complete:-command-::commands' ignored-patterns '*\~'

# Separate matches into groups
zstyle ':completion:*:matches' group 'yes'

# Describe each match group.
zstyle ':completion:*:descriptions' format "%B---- %d%b"

# Messages/warnings format
zstyle ':completion:*:messages' format '%B%U---- %d%u%b' 
zstyle ':completion:*:warnings' format '%B%U---- no match for: %d%u%b'

# Describe options in full
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'

zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes

users=(lee lbrinton)

zstyle ':completion:*' users $users

hosts=(
    localhost
    #sam.leebrinton.net
    #frodo.leebrinton.net
    #strider.leebrinton.net
    #elrond.leebrinton.net
    #mail.leebrinton.net
    )

zstyle ':completion:*' hosts $hosts

zmodload -i zsh/complist
zstyle ':completion:*' list-colors ''
zstyle ':completion:*:*:kill:*:processes' list-colors \
       '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*' list-colors "$LS_COLORS"

# All my accounts:
#my_accounts=(
#    {lee,root}@frodo.leebrinton.net
#    (lee,root)@sam.leebrinton.net
#    (lee,root)@elrond.leebrinton.net
#)

zstyle ':completion:*:my-accounts' users-hosts $my_accounts
#zstyle ':completion:*:other-accounts' users-hosts $other_accounts

#telnet_users_hosts_ports=(
#    user1@host1:
#    user2@host2:
#    @mail-server:{smtp,pop3}
#    @news-server:nntp
#    @proxy-server:8000
#  )
#zstyle ':completion:*:*:telnet:*' users-hosts-ports
#$telnet_users_hosts_ports

autoload zrecompile
unalias run-help
autoload run-help
autoload -U edit-command-line
zle -N edit-command-line

autoload zcalc

#---------------------------------------------------------------------------
# Aliases
#---------------------------------------------------------------------------
alias su='su - '
alias pix='nocorrect pix'
alias git='nocorrect git'

#---------------------------------------------------------------------------
# Emulate ms-dos path command
#---------------------------------------------------------------------------
alias path='echo $PATH'         # ms-dos path command
alias cls=clear

#---------------------------------------------------------------------------
# Alias shortcuts for ls
#---------------------------------------------------------------------------
case $OSTYPE in
    openbsd*) alias ls='ls -F';;
    freebsd*) alias ls='ls -F';;
    darwin*) alias ls='ls -F';;
    *) alias ls='ls -F --color=auto';;   # Use colors
esac    

alias l='ls -CF'                       # shorthand for list
alias ll='ls -lh'                         # shorthand for long list

if [[ -x /usr/bin/d ]]
then
  alias lsd='d'
else
  alias lsd='ls -ldh'                        
fi

# MvMac is a pain in the ass
#if [[ "$RUNNING_DARWIN" = 'true' ]]
#then
#    alias cp=/Developer/Tools/CpMac
#    alias mv=/Developer/Tools/MvMac
#fi

alias p=popd
alias df='df -h'
alias du='du -h'
alias hup='kill -HUP'

#---------------------------------------------------------------------------
# functions
#---------------------------------------------------------------------------
# csh compatibility
setenv () { typeset -x "${1}${1:+=}${(@)argv[2,$#]}" }


# functions for displaying neat stuff in *term title
case $TERM in
    *xterm*|rxvt|(dt|k|E)term)
    ## display user@host and full dir in *term title
    precmd ()
    {
	print -Pn  "\033]0;%n@%m %~\007"
	#print -Pn "\033]0;%n@%m%#  %~ %l  %w :: %T\a" ## or use this
    }
    ## display user@host and name of current process in *term title
    preexec ()
    {
	print -Pn "\033]0;%n@%m <$1> %~\007"
	#print -Pn "\033]0;%n@%m%#  <$1>  %~ %l  %w :: %T\a"
    }
    ;;
esac

## invoke this every time when u change .zshrc to
## recompile it.
src ()
{
	autoload -U zrecompile
	[ -f ~/.zshrc ] && zrecompile -p ~/.zshrc
	[ -f ~/.zcompdump ] && zrecompile -p ~/.zcompdump
	[ -f ~/.zshrc.zwc.old ] && rm -f ~/.zshrc.zwc.old
	[ -f ~/.zcompdump.zwc.old ] && rm -f ~/.zcompdump.zwc.old
	source ~/.zshrc
}


#---------------------------------------------------------------------------
# emacs
# Run gnu emacs my way
#---------------------------------------------------------------------------
emacs()
{
    local geometry='--geometry=80x50'

    if [[ $RUNNING_CYGWIN == true && -d $EMACSHOME ]]
    then
        prg="$EMACSHOME/bin/runemacs"
    elif [[ $RUNNING_DARWIN == true && -d /Applications/MacPorts/Emacs.app ]]
    then
        prg=/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs
        geometry=''
    elif [[ $RUNNING_DARWIN == true && -d /Applications/Emacs.app ]]
    then
        prg=/Applications/Emacs.app/Contents/MacOS/Emacs
    else
        prg=emacs
    fi
# command $prg "-bg 'light yellow' -cr salmon -g 80x57 $@"
# This works on Cygwin
    command $prg -bg 'light yellow' -cr salmon $geometry $@
}

#---------------------------------------------------------------------------
# Find process to kill and kill it.
#---------------------------------------------------------------------------
pskill ()
{ 
	local pid
    
    # Find it
	pid=$(ps -ax | grep $1 | grep -v grep | awk '{ print $1 }')

    # Kill it
	echo -n "killing $1 (process $pid)..."
	kill -9 $=pid
	echo "slaughtered."
}

#---------------------------------------------------------------------------
# findsrc - find source files
#
#   Find all java, xml, and sql files in the current directory and children.
#
#  Another way to do this:
#    find . -type f -a 
#      \( -name '*.java' -o -name '*.xml' -o -name '*.sql' \) -print
#---------------------------------------------------------------------------
findsrc ()
{
  if [[ $RUNNING_DARWIN == true ]]
  then
    find . -name .svn -prune -or \
           \( -type f -and \
           \( -name '*.java' -or -name '*.xml' -or -name '*.sql' \) \) -print
  else
    find . -type f -a -regex '.*\.\(java\|xml\|sql\)$' -print
  fi
}

#---------------------------------------------------------------------------
# srcsearch - find source files and search them for a regex
#---------------------------------------------------------------------------
srcsearch ()
{
  findsrc | xargs grep -i $1
}

#---------------------------------------------------------------------------
# sshto
# Login the a remote host using ssh
sshto()
{
    local ids
    
    ids=$(ssh-add -l)

    if [[ $ids == 'The agent has no identities.' ]]
    then
        ssh-add
    fi

    if [[ $RUNNING_CYGWIN == true ]]
    then
      ssh -Y $1
    else
      ssh -X $1
    fi
}

#---------------------------------------------------------------------------
# sam
# Login sam.leebrinton.net using ssh
#---------------------------------------------------------------------------
sam()
{
    sshto lee@sam
}

#---------------------------------------------------------------------------
# bilbo
# Login sam.leebrinton.net using ssh
#---------------------------------------------------------------------------
bilbo()
{
    sshto lee@bilbo
}

#---------------------------------------------------------------------------
# elrond
# Login elrond.leebrinton.net using ssh
#---------------------------------------------------------------------------
elrond()
{
    sshto lee@elrond
}

#---------------------------------------------------------------------------
# setdisplay
# Function to set the DISPLAY and HOSTDISPLAY environment variables
#---------------------------------------------------------------------------
setdisplay()
{
    export DISPLAY=${1}:0.0
    export HOSTDISPLAY=${DISPLAY}
}
#---------------------------------------------------------------------------
# tgz
# tars and gzips a list of files/directories
# usage: tgz archive dir1 dir2...
# creates archive.tar.gz
#---------------------------------------------------------------------------
tgz()
{
    tarball=${1}.tar
    archive=${1}.tar.gz
    echo packing ${archive}
    shift
    tar cf ${tarball} $@
    gzip ${tarball}
}

#---------------------------------------------------------------------------
# untgz
# unpacks a list of .tgz .tar.gz or tar.Z archives
#---------------------------------------------------------------------------
untgz()
{
    for archive in $@
    do
	echo unpacking ${archive}
        tar zxvf ${archive} 
    done
}

#---------------------------------------------------------------------------
# lstgz
# List the files in a set of archives
#---------------------------------------------------------------------------
lstgz()
{
    for archive in $@
    do
        echo listing ${archive}
        tar ztf ${archive}
    done
}

#---------------------------------------------------------------------------
# untar
# extract files from a tar archive 
#---------------------------------------------------------------------------
untar()
{
    case ${1} in
        *gz) tar zxf ${1};;
        *bz2) btar xf ${1};;
        *tar) tar xf ${1};;
    esac
}

#---------------------------------------------------------------------------
# btar - Use bzip2 as the compession progam
#---------------------------------------------------------------------------
btar()
{
    tar $@ --use-compress-program /usr/bin/bzip2
}

#---------------------------------------------------------------------------
# sd
# select directory from dirs
#---------------------------------------------------------------------------
sd()
{
    PS3='directory (0 to stay put)? '

    select selection in $(dirs)
    do
        if [[ -n $selection ]]
        then
            cd $selection
            break
        else
            echo 'Staying put.'
            break
        fi
    done
}

#---------------------------------------------------------------------------
# lines
# Print the number of lines in file(s)
#---------------------------------------------------------------------------
lines()
{
    wc -l $@
}

#---------------------------------------------------------------------------
# Project stuff
#---------------------------------------------------------------------------
if [[ -d /cygdrive/c/dev ]]
then
    export dev=/cygdrive/c/dev
    export sesco=/cygdrive/c/SESCO
fi

#---------------------------------------------------------------------------
# Set the main prompt
#---------------------------------------------------------------------------
if [[ $TERM == cygwin ]]
then
    prompt adam2
elif [[ $TERM == linux ]]
then
    prompt adam2
elif [[ $RUNNING_DARWIN == true ]]
then
    prompt adam1
elif [[ $RUNNING_CYGWIN == true ]]
then
    prompt adam2
elif [[ -n KONSOLE_DCOPRef ]]
then
    prompt oliver yellow black 
else
    #prompt oliver yellow black
    prompt adam2
fi

############################################################################
# End of zshrc
############################################################################


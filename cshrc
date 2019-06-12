##### vim:set filetype=csh : ##################################################
#
# $HOME/config/cshrc
#
# This file contains startup commands common to all
# C shell derived interactive shellls including:
# csh and tcsh.
#
###############################################################################

#******************************************************************************
# System stuff
#******************************************************************************
#------------------------------------------------------------------------------
# Set the shell process's file creation mask
#------------------------------------------------------------------------------
umask 022

#---------------------------------------------------------------------------
# Set LANG environment variable.
#---------------------------------------------------------------------------
if (-x /usr/bin/locale) then
    set lang_str = `/usr/bin/locale | grep LANG`
    set split = ($lang_str:as/=/ /)
    if ($#split == 2) then
        set export_lang_str = "setenv LANG $split[2]"
        eval "$export_lang_str"
        unset export_lang_str
    endif
    unset lang_str split
endif

#---------------------------------------------------------------------------
# Set the RUNNING_CYGWIN, RUNNING_MINGW, RUNNING_MSYS2
# and RUNNING_DARWIN environment variables.
#---------------------------------------------------------------------------
setenv RUNNING_CYGWIN          'false'
setenv RUNNING_DARWIN          'false'
setenv RUNNING_LINUX           'false'
setenv RUNNING_MSYS2           'false'
setenv RUNNING_MINGW32         'false'
setenv RUNNING_MINGW64         'false'
setenv RUNNING_MSYS2_BASED_ENV 'false'
setenv RUNNING_FREEBSD         'false'

switch (`uname`)
    case CYGWIN*:
        setenv RUNNING_CYGWIN 'true'
        breaksw

    case Darwin:
        setenv RUNNING_DARWIN 'true'
        breaksw

    case Linux:
        setenv RUNNING_LINUX 'true'
        breaksw

    case MINGW32*:
        setenv RUNNING_MINGW32 'true'
        breaksw

    case MINGW64*:
        setenv RUNNING_MINGW64 'true'
        breaksw

    case MSYS_NT*:
        setenv RUNNING_MSYS2 'true'
        breaksw

    case FreeBSD:
        setenv RUNNING_FREEBSD 'true'
        breaksw
endsw

if ("$RUNNING_MINGW32" == 'true' || \
    "$RUNNING_MINGW64" == 'true' || \
    "$RUNNING_MSYS2"   == 'trun') then
    setenv RUNNING_MSYS2_BASED_ENV 'true'
endif

#---------------------------------------------------------------------------
# Set ENV to a file invoked each time sh is started for interactive use.
#---------------------------------------------------------------------------
if (-e ${HOME}/.shrc) then
  setenv ENV ${HOME}/.shrc
endif

#---------------------------------------------------------------------------
# Set the HOSTNAME environment variable.
#---------------------------------------------------------------------------
if (! $?HOSTNAME) then
    setenv  HOSTNAME `hostname`
endif

#------------------------------------------------------------------------------
# Set the temporary directory
#
# If /tmp exists and is a directory
#	Set TMP to /tmp
#------------------------------------------------------------------------------
if (-d /tmp) then
    setenv TMP '/tmp'
endif


#------------------------------------------------------------------------------
# Set the JAVA_HOME environment variable.
# Everything java depends on this.
#------------------------------------------------------------------------------
if ($RUNNING_CYGWIN == 'true') then
  if (-d '/cygdrive/c/Program Files/Java/jdk1.8.0_45') then
    setenv JAVA_VERSION '1.8.0_45'
    setenv JAVA_HOME 'c:\\Program Files\\Java\\jdk1.8.0_45'
  endif
else if ($RUNNING_DARWIN == 'true') then
  if (-d /Library/Java/JavaVirtualMachines) then
    set all_vm_list = (jdk1.8.0_45/Contents/Home jdk1.8.0_66.jdk/Contents/Home)
    foreach vm ($all_vm_list)    
      set p = /Library/Java/JavaVirtualMachines/${vm}     
      if (-d $p) then
        setenv JAVA_HOME ${p}
      endif
    end
    unset all_vm_list p
  else if (-d /Library/Java/Home) then
    setenv JAVA_HOME /Library/Java/Home 
  endif
else if (-d /usr/lib/jvm/default-java) then
  setenv JAVA_HOME '/usr/lib/jvm/default-java'
else if (-d /usr/lib/jvm/java-6-openjdk) then
  setenv JAVA_VERSION '1.6.0'
  setenv JAVA_HOME '/usr/lib/jvm/java-6-openjdk'
endif

#------------------------------------------------------------------------------
# Set the PATH environment variable.
# Colon separated list of directories in which the shell looks for commands
#------------------------------------------------------------------------------
#
# Initialize variables
#
if (-x /usr/bin/whoami) then
    set realuser = `whoami`
else
    set realuser = ${LOGNAME}
endif

#
# If user is root 
#
if ($realuser == 'root') then
  set all_paths = (/usr/local/bin /usr/local/sbin /sbin /bin \
                   /usr/sbin /usr/bin /usr/bin/X11 /usr/X11R6/bin)
else if ($RUNNING_DARWIN == 'true') then
  set all_paths = (/usr/local/sbin /usr/local/bin \
                   /opt/local/bin /opt/local/sbin \
                   /sw/sbin /sw/bin /sbin /bin /usr/sbin /usr/bin \
                   /usr/bin/X11 /usr/X11R6/bin \
                   /Developer/Tools /DeveloperApplications/Utilities \
                   /usr/local/mysql/bin .)
else
  set all_paths = (/usr/local/sbin /usr/local/bin \
                   /opt/bin /sbin /bin /usr/sbin /usr/bin \
                   /usr/bin/X11 /usr/X11R6/bin /usr/games .)
endif
unset realuser

foreach p ($all_paths)
  if (-d "${p}") then
    if ($?tpath) then
      set tpath = "${tpath}:${p}"
    else
      set tpath = "${p}"
    endif
  endif
end
unset p all_paths

#
# If ${HOME}/bin exists add it to the path
#
if ($?HOME && -d ${HOME}/bin) then
    set tpath = "${HOME}/bin":"$tpath"
endif

#
# If JAVA_HOME is set add the java bin dir to the path
#
if ($?JAVA_HOME) then
  if ($RUNNING_CYGWIN == 'true') then
    set tpath = `cygpath -u $JAVA_HOME/bin`:"$tpath"
  else
    set tpath = "$JAVA_HOME/bin":"$tpath"
  endif
endif

#
# if ANT_HOME exists add it
#
set all_ant_versions= (\
'1.8.2' \
'1.9.4' \
1.10.1)

foreach ant_version ($all_ant_versions)
  if (-d /usr/local/apache-ant-${ant_version}/bin) then
    set tpath = "$tpath":"/usr/local/apache-ant-${ant_version}/bin"
  endif
end
unset ant_version all_ant_versions

#
# if ORACLE_HOME exists add it
#
if ($?ORACLE_HOME) then
    if ($RUNNING_CYGWIN == 'true') then
        set tpath = "$tpath":`cygpath -u $ORACLE_HOME` 
    else
        set tpath = "$tpath":"$ORACLE_HOME"
    endif
endif

#
# if running under cygwin add window system directories
#
if ($RUNNING_CYGWIN == 'true') then
  set tpath = "$tpath":`cygpath -u $WINDIR`

  if (-d `cygpath -u $WINDIR/system`) then
    set tpath = "$tpath":`cygpath -u $WINDIR/system`
        endif

  if (-d `cygpath -u $WINDIR/system32`) then
    set tpath = "$tpath":`cygpath -u $WINDIR/system32`
  endif

  if (-d `cygpath -u $WINDIR/system32/Wbem`) then
    set tpath = "$tpath":`cygpath -u $WINDIR/system32/Wbem`
    endif
endif

setenv PATH "$tpath"
unset tpath

#------------------------------------------------------------------------------
# Readline configuration
#------------------------------------------------------------------------------
if (-f $HOME/.inputrc) then
    setenv INPUTRC "$HOME/.inputrc"
else if (-f /etc/inputrc) then
    setenv INPUTRC /etc/inputrc
endif

#------------------------------------------------------------------------------
# Terminal definition
#------------------------------------------------------------------------------
if (! $?TERM) setenv TERM 'linux'

if ("$RUNNING_DARWIN" == 'true') then
    if ($?TERM_PROGRAM && $TERM_PROGRAM == 'iTerm.app') then
        echo "Running in ${TERM_PROGRAM} ${TERM_PROGRAM_VERSION}"
    endif


    if ($TERM == 'xterm') then
        setenv TERM 'xterm-color'
    endif
endif

#------------------------------------------------------------------------------
# Setup the LS_COLORS environment variable
#------------------------------------------------------------------------------
if (-x /usr/bin/dircolors && -f ~/config/dircolors) then
    eval `dircolors --c-shell ~/config/dircolors`
endif

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
# pipe     = yell	ow
# exe      = bold red
# block sp = blue on cyan
# char sp  = blue on brown
# setuid x = black on red
# setgid x = black on cyan
# dir o+ws = black on green
# dir o+w  = black on brown

setenv CLICOLOR 'ON'
setenv LSCOLORS 'ExFxCxDxBxegedabagacad'
#
# Causes ls-F to show symbolic links with:
# @ link to a non-directory
# > link to a directory
# & link to no where
#
set listlinks = 'ON'

#
# We have ls aliased to ls-F a tcsh builtin. When you execute ls-F with any
# switchs it passes its arguments to /bin/ls.  On *BSD machines (includeing
# darwin) adding ls to the color variable causes a switch to be passed to
# ls that bsd ls does not support :(
#
if ($RUNNING_CYGWIN == 'true') then
  set color = (ls-F ls)
else
  set color = (ls-F)
endif

#------------------------------------------------------------------------------
# Set LOCATE_PATH - A colon separated list of database file names by the
# locate command.
#------------------------------------------------------------------------------
if ($RUNNING_CYGWIN == 'true') then
    setenv LOCATE_PATH '/usr/var/locatedb'
endif

#------------------------------------------------------------------------------
# Set CDPATH - A colon separated list of directories used as a
# search path for 'cd'
#------------------------------------------------------------------------------
set cdpath = (. ~)

set fignore = (.o \~ .class .\$\$\$)

#---------------------------------------------------------------------------
# Set CPATH - A colon separated list of directories searched
# for include files.
#---------------------------------------------------------------------------
setenv CPATH '/usr/include'

if (-d /opt/local/include) then
    setenv CPATH "/opt/local/include:${CPATH}"
endif

if (-d /usr/local/include) then
    setenv CPATH "/usr/local/include:${CPATH}"
endif

#---------------------------------------------------------------------------
# Set LIBRARY_PATH - A colon separated list of directories searched
# for libraries.
#---------------------------------------------------------------------------
set libpath = ''

if (-d /lib) then
    set libpath = "$libpath":'/lib'
endif

if (-d /usr/lib) then
    set libpath = "$libpath":'/usr/lib'
endif

if (-d /opt/local/lib) then
    set libpath = '/opt/local/lib':"$libpath"
endif

if (-d /usr/local/lib) then
    set libpath = '/usr/local/lib':"$libpath"
endif
setenv LIBRARY_PATH "$libpath"
unset libpath

#---------------------------------------------------------------------------
# Set LD_LIBRARY_PATH - A colon separated list of directories searched
# for dynamic libraries.
#---------------------------------------------------------------------------
if ( "$RUNNING_LINUX" == 'true' || "$RUNNING_CYGWIN" == 'true' ) then
  if (-d /usr/local/lib) then
    setenv LD_LIBRARY_PATH "/usr/local/lib:${LD_LIBRARY_PATH}"
  endif
endif

#---------------------------------------------------------------------------
# Set DYLD_LIBRARY_PATH - A colon separated list of directories searched
# for dynamic libraries.
#---------------------------------------------------------------------------
#if ( "$RUNNING_DARWIN" == 'true') then
#  setenv DYLD_LIBRARY_PATH '/usr/lib'
#
#  if (-d /opt/local/lib) then
#    setenv DYLD_LIBRARY_PATH "/opt/local/lib:${DYLD_LIBRARY_PATH}"
#  endif
#
#  if (-d /usr/local/lib) then
#    setenv DYLD_LIBRARY_PATH "/usr/local/lib:${DYLD_LIBRARY_PATH}"
#  endif
#endif

#------------------------------------------------------------------------------
# Set the default pager
#------------------------------------------------------------------------------
if (-x /usr/bin/less) then
    setenv PAGER 'less'

    ###########################################################################
    #
    # Set environment variables for less
    #
    ###########################################################################
    #--------------------------------------------------------------------------
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
    #--------------------------------------------------------------------------
    setenv LESS '-eisnmQRs -x4'
else
    setenv PAGER 'more'
endif

if (-x '/usr/bin/more') then
    setenv MORE '-erX'
endif

#------------------------------------------------------------------------------
# Set the default editor
#------------------------------------------------------------------------------
setenv EDITOR 'vi'
setenv VISUAL 'vi'

#---------------------------------------------------------------------------
# Set the location of the artistic style option file
#---------------------------------------------------------------------------
if (-f "${HOME}/config/astylerc") then
   setenv ARTISTIC_STYLE_OPTIONS "${HOME}/config/astylerc"
endif

#---------------------------------------------------------------------------
# Set XNLSPATH - X Locales
#---------------------------------------------------------------------------
if (-d /usr/X11R6/lib/X11/locale) then
    setenv XNLSPATH '/usr/X11R6/lib/X11/locale'
else if (-d /usr/X11R6/lib/X11/nls) then
    setenv XNLSPATH '/usr/X11R6/lib/X11/nls'
endif

#------------------------------------------------------------------------------
# Set the sgml catalog and xml catalog
#------------------------------------------------------------------------------
unset sgml_cat_files xml_cat_files

 set all_sgml_catalogs = (/etc/sgml/catalog \
                          /opt/local/etc/sgml/catalog \
                          /usr/local/share/sgml/catalog \
                          /usr/share/openjade/catalog \
                          /usr/share/xml/docbook/4.3/docbook.cat \
                          /usr/local/share/docbook-dsssl/catalog)

set all_opensp_dirs = (/usr/share/OpenSP /opt/local/share/OpenSP)

foreach cat ($all_sgml_catalogs)
  if (-f $cat) then
    if ($?sgml_cat_files) then
      set sgml_cat_files = "${sgml_cat_files}:${cat}"
    else
      set sgml_cat_files = "${cat}"
    endif
  endif
end
unset all_sgml_catalogs cat

foreach opensp ($all_opensp_dirs)
  if (-d $opensp) then
    foreach cat (`ls ${opensp}/*.soc`)
      if ($?sgml_cat_files) then
        set sgml_cat_files = "${sgml_cat_files}:${cat}"
      else
        set sgml_cat_files = "${cat}"
      endif
    end
  endif
end
unset all_opensp_dirs opensp

if ($?sgml_cat_files) then
  setenv SGML_CATALOG_FILES "$sgml_cat_files"
endif
unset cat sgml_cat_files

set all_xml_catalogs = (/etc/xml/catalog \
                        /opt/local/etc/xml/catalog /usr/local/lib/xml/catalog)

foreach cat ($all_xml_catalogs)
  if (-f $cat) then
    if ($?xml_cat_files) then
      set xml_cat_files = ("${xml_cat_files} ${cat}")
    else
      set xml_cat_files = "${cat}"
    endif
  endif
end
unset cat all_xml_catalogs

if ($?xml_cat_files) then
  setenv XML_CATALOG_FILES "$xml_cat_files"
endif
unset xml_cat_files

#------------------------------------------------------------------------------
# Set up html tidy
#------------------------------------------------------------------------------
setenv HTML_TIDY "~/config/tidy"

#------------------------------------------------------------------------------
# Set the MANPATH environment variable.
# A colon separated list of directories in which to search for man files
#------------------------------------------------------------------------------
if (-d /opt/local/share/man) then
    setenv MANPATH "${MANPATH}:/opt/local/share/man"
endif

#------------------------------------------------------------------------------
# Set the INFOPATH environment variable.
# A colon separated list of directories in which to search for info 'dir' files
#------------------------------------------------------------------------------
setenv INFOPATH '.'

set all_info_paths = (/opt/local/share/info \
                      /usr/local/share/info /usr/local/info \
                      /usr/share/info /usr/info \
                      /Applications/MacPorts/Emacs.app/Contents/Resources/info)

foreach p ($all_info_paths)
  if (-d ${p}) then
    setenv INFOPATH "${INFOPATH}:${p}"
  endif
end
unset p all_info_paths

#------------------------------------------------------------------------------
# Set the SYSSCREENRC variable location of the system level screen config file.
#------------------------------------------------------------------------------
if ($RUNNING_CYGWIN == 'true') then
    setenv SYSSCREENRC '/etc/screenrc'
endif

#------------------------------------------------------------------------------
# Build a new signature file
#------------------------------------------------------------------------------
if (-x /usr/bin/signify) then
    signify > "~/.signature"
endif

#-----------------------------------------------------------------------------
# Fortune
#-----------------------------------------------------------------------------
if ($RUNNING_FREEBSD == 'true') then
    if (-x /usr/games/fortune) then
        /usr/games/fortune freebsd-tips
    endif
endif

#------------------------------------------------------------------------------
# Start in the users home directory
#------------------------------------------------------------------------------
if ("$HOME" != "$PWD") then
    cd "$HOME"
endif

#---------------------------------------------------------------------------
# Setup ssh-agent
#---------------------------------------------------------------------------
if ($RUNNING_CYGWIN == 'true') then
    echo 'Loading ssh keys ...'
    
    foreach key (id_rsa id_dsa trexone_rsa) 
        if (-e ~/.ssh/$key) then
            echo "$key"
            keychain "$HOME/.ssh/$key"
            source "$HOME/.keychain/$HOSTNAME-csh" > /dev/null
        endif
    end 
endif

#------------------------------------------------------------------------------
# set blocksize
#------------------------------------------------------------------------------
setenv BLOCKSIZE K

#------------------------------------------------------------------------------
# set keys
#------------------------------------------------------------------------------
bindkey '^[[3~' delete-char
bindkey '^[r'   history-search-backward
bindkey '^[f'   history-search-forward

#------------------------------------------------------------------------------
# Set aliases
#------------------------------------------------------------------------------
alias su 'su - '

#---------------------------------------------------------------------------
# Emulate ms-dos path command
#---------------------------------------------------------------------------
alias path  echo $PATH
alias cls   clear

#---------------------------------------------------------------------------
# Alias shortcuts for ls
#
# -a Include directory entries whose names begin with a dot (.)
# -A List all entries except for . and ..
# -C Force multi-column output; the default when output is to a terminal
# -d Directories are listed as plain files (not searched recursivety)
# -F Mark dir, exe, symlink, and special file types
# -h Human readable and reduced digits to three or less
#---------------------------------------------------------------------------
alias ls ls-F
alias l	ls -CF
alias la ls -aF
alias ll ls -lhAF

if (-x /usr/bin/d) then
  alias lsd d
else
  alias lsd ls -ldh
endif

alias btar  tar --use-compress-program /usr/bin/bzip2
alias cd    pushd
alias p     popd
alias df	df -h
alias du	du -h
alias h		'history -r | less'
alias j		jobs -l
alias tset	'set noglob histchars=""; eval `\tset -s \!*`; unset noglob histchars'
alias z		suspend
alias hup   kill -HUP

if ($?prompt) then
    # An interactive shell -- set some stuff up
    set autocorrect
    set autoexpand
    set autolist = ambiguous
    set autorehash
    set complete = enhance
    set correct = cmd
    set edit
    set filec
    set history = 1000
    set savehist = (1000 merge)
    set ignoreeof
    set mail = (/var/mail/$USER)
   
    if (-x /usr/bin/fortune) then
        if ($RUNNING_FREEBSD == 'true') then
            /usr/bin/fortune freebsd-tips
        endif
    endif

    if ($?tcsh) then
        bindkey "^W" backward-delete-word
        #bindkey -k up history-search-backword
        #bindkey -k down history-search-forward
    endif
    
    set ellipsis
    set promptchars = '%#'
    switch ($TERM)
    case "xterm*":
        set prompt = '%{\033]0;%n@%m:%~\007%}[%B%n@%m%b] %B%~%b%# '
        breaksw

    default:
	set prompt = '[%B%n@%m%b] %B%~%b%# '
        #set prompt = '%n@%m:%c3%# '
        breaksw
    endsw
endif


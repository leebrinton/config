##### vim:set filetype=fish : #################################################
#
# config.fish
#
# This file contains startup commands for fish the friendly interactive shell.
#
# Link to this fille from ~/.config/fish/config.fish      
#
###############################################################################

#******************************************************************************
# System stuff
#******************************************************************************
#------------------------------------------------------------------------------
# Set the shell process's file creation mask
#------------------------------------------------------------------------------
set umask 022

#---------------------------------------------------------------------------
# Set LANG environment variable.
#---------------------------------------------------------------------------
if test -x /usr/bin/locate
    set -l lang_str (/usr/bin/locale | grep LANG)
    set -l tokens (string split '=' "$lang_str")
    set -l num_tokens (count $tokens)

    if test $num_tokens -eq 2
        set -l export_lang_str "set -x LANG $tokens[2]"
        eval "$export_lang_str"
    end
end

#---------------------------------------------------------------------------
# Set the RUNNING_CYGWIN, RUNNING_MINGW, RUNNING_MSYS2
# and RUNNING_DARWIN environment variables.
#---------------------------------------------------------------------------
set -x RUNNING_CYGWIN false
set -x RUNNING_MSYS2 false
set -x RUNNING_MINGW32 false
set -x RUNNING_MINGW64 false
set -x RUNNING_MSYS2_BASED_ENV false
set -x RUNNING_DARWIN false
set -x RUNNING_FREEBSD false

switch (uname)
  case 'CYGWIN*'
    set -x RUNNING_CYGWIN true
  case 'Darwin*'
    set -x RUNNING_DARWIN true
  case 'MINGW32*'
    set -x RUNNING_MINGW32 true
  case 'MINGW64*'
    set -x RUNNING_MINGW64 true
  case 'MSYS_NT*'
    set -x RUNNING_MSYS2 true
  case 'FreeBSD*'
    set RUNNING_FREEBSD true
end

if test "$RUNNING_MINGW32" = 'true' -o \
     "$RUNNING_MINGW64" = 'true' -o "$RUNNING_MSYS2" = 'true' 
    set -x RUNNING_MSYS2_BASED_ENV true
end

#
# MSYS2 is really 3 different environments, it uses environment vars to define
# these environments. Use their profile to generate the vars.
#
if test "$RUNNING_MSYS2_BASED_ENV" = 'true'
    . /etc/profile
end

#---------------------------------------------------------------------------
# Set the HOSTNAME environment variable.
#---------------------------------------------------------------------------
if test -z "$HOSTNAME"
    set -x HOSTNAME (hostname)
end

#------------------------------------------------------------------------------
# Set the temporary directory
#
# If TMP is undefined and /tmp exists and is a directory
#   Set TMP to /tmp
#------------------------------------------------------------------------------
if test -z "$TMP"
    if test -d /tmp
        set -x TMP '/tmp'
    end
end

#------------------------------------------------------------------------------
# Set the JAVA_HOME environment variable.
# Everything java depends on this.
#------------------------------------------------------------------------------
if test "$RUNNING_CYGWIN" = 'true'
    set -l java_version '1.8.0_45'
    set -l test_path "/cygdrive/c/Program Files/Java/jdk$java_version"
    if test -d "$test_path"
        set -x JAVA_HOME (cygpath --mixed "$test_path")
    end
else if test "$RUNNING_MSYS2_BASED_ENV" = 'true'
    set -l java_version '1.8.0_45'
    set -l base_path 'Program Files/Java/jdk'
    set -l test_path "/c/$base_path$java_version"
    if test -d "$test_path" 
        set -x JAVA_HOME "C:/$base_path$java_version"
    end
else if test "$RUNNING_DARWIN" = 'true'
    if test -d /Library/Java/JavaVirtualMachines
        set -l all_vm_list 'jdk1.8.0_45.jdk/Contents/Home'
        set -l all_vm_list $all_vm_list 'jdk1.8.0_66.jdk/Contents/Home'

        for vm in $all_vm_list
            set -l p "/Library/Java/JavaVirtualMachines/$vm"
            if test -d "$p"
                set -x JAVA_HOME "$p"
            end
        end
        set -e all_vm_list
    else if test -d /Library/Java/Home
        set -x JAVA_HOME '/Library/Java/Home'
    end
else if test -d '/usr/lib/jvm/default-java'
    set -x JAVA_HOME '/usr/lib/jvm/default-java'
else if test -d '/usr/lib/jvm/java-6-openjdk'
    set -x JAVA_HOME '/usr/lib/jvm/java-6-openjdk'
end

#------------------------------------------------------------------------------
# Set the GOROOT and GOPATH environment variable
#------------------------------------------------------------------------------
if test -d '/opt/local/go'
    set -x GOROOT '/opt/local/go'
end

if test -d "~/go"
    set GOPATH "~/go"
end

#------------------------------------------------------------------------------
# Set the PATH environment variable.
# A list of directories in which the shell looks for commands
#------------------------------------------------------------------------------
#
# Initialize variables
#
if test -x /usr/bin/whoami
  set self (whoami)
else
  set self $LOGNAME
end

set -e PATH

#
# If user is root 
#
if test "$self" = 'root'
    set all_paths '/usr/local/bin'
    set all_paths $all_paths /usr/local/sbin
    set all_paths $all_paths /sbin
    set all_paths $all_paths /bin
    set all_paths $all_paths /usr/sbin
    set all_paths $all_paths /usr/bin
    set all_paths $all_paths /usr/bin/X11
    set all_paths $all_paths /usr/X11R6/bin
    else if test "$RUNNING_DARWIN" = 'true'
    set all_paths /usr/local/sbin
    set all_paths $all_paths /usr/local/bin
    set all_paths $all_paths /opt/local/bin
    set all_paths $all_paths /opt/local/sbin
    set all_paths $all_paths /sbin
    set all_paths $all_paths /bin
    set all_paths $all_paths /usr/sbin
    set all_paths $all_paths /usr/bin
    set all_paths $all_paths /usr/bin/X11
    set all_paths $all_paths /usr/X11R6/bin
    set all_paths $all_paths /Developer/Tools
    set all_paths $all_paths /Developer/Applications/Utilities
    set all_paths $all_paths /usr/local/mysql/bin
    set all_paths $all_paths .
else
    set all_paths /usr/local/sbin
    set all_paths $all_paths /usr/local/bin
    set all_paths $all_paths /opt/bin
    set all_paths $all_paths /sbin
    set all_paths $all_paths /bin
    set all_paths $all_paths /usr/sbin
    set all_paths $all_paths /usr/bin
    set all_paths $all_paths /mingw/bin
    set all_paths $all_paths /usr/bin/X11
    set all_paths $all_paths /usr/X11R6/bin
    set all_paths $all_paths /usr/games
    set all_paths $all_paths .
end
set -e self

for p in $all_paths
  if test -d "$p"
    set PATH $PATH $p
  end
end
set -e p
set -e all_paths

#
# If ~/bin exists add it to the path
#
if test -d "$HOME/bin"
    set PATH $PATH "$HOME/bin"
end

#
# If JAVA_HOME is set add the java bin dir to the path
#
if test -n "$JAVA_HOME"
    if test "$RUNNING_CYGWIN" = 'true'
        set PATH $PATH (cygpath -u "$JAVA_HOME/bin")
    else if test "$RUNNING_MSYS2_BASED_ENV" = 'true'
        set PATH $PATH (cygpath -u "$JAVA_HOME/bin")
    else
        set PATH $PATH "$JAVA_HOME/bin"
    end
end

#
# if ANT_HOME/bin exists add it
#
set all_ant_versions 1.10.1
set all_ant_versions $all_ant_versions 1.9.4
set all_ant_versions $all_ant_versions 1.8.2

for ant_version in $all_ant_versions
    if test -d "/usr/local/apache-ant-$ant_version"
        set PATH $PATH "/usr/local/apache-ant-$ant_version/bin"
    else if test -d "/c/cygwin/usr/local/apache-ant-$ant_version/bin"
        set PATH $PATH "/c/cygwin/usr/local/apache-ant-$ant_version/bin"
    end
end
set -e ant_version
set -e all_ant_versions

#
# if running under cygwin add windows system 
#
if test "$RUNNING_CYGWIN" = 'true'
    if test -d (cygpath -u $WINDIR)
        set PATH $PATH (cygpath -u $WINDIR) 
    end

    if test -d (cygpath -u $WINDIR/system)
        set PATH $PATH (cygpath -u $WINDIR/system)
    end

    if test -d (cygpath -u $WINDIR/system32)
        set PATH $PATH (cygpath -u $WINDIR/system32)
    end

    if test -d (cygpath -u $WINDIR/system32/Wbem)
        set PATH $PATH (cygpath -u $WINDIR/system32/Wbem)
    end
else if test "$RUNNING_MSYS2_BASED_ENV" = 'true'
    set PATH $PATH $ORIGINAL_PATH

    if test -d /usr/bin/site_perl
        set PATH $PATH /usr/bin/site_perl
    end

    if test -d /usr/bin/vendor_perl
        set PATH $PATH /usr/bin/vendor_perl
    end

    if test -d /usr/bin/core_perl
        set PATH $PATH /usr/bin/core_perl
    end
    
    if test "$RUNNING_MINGW32" = 'true'
        set PATH /mingw32/bin $PATH
    end

    if test "$RUNNING_MINGW64" = 'true'
        set PATH /mingw64/bin $PATH
    end
end
set -x PATH $PATH

#------------------------------------------------------------------------------
# Readline configuration
#------------------------------------------------------------------------------
if test -f "~/.inputrc"
  set -x INPUTRC "~/.inputrc"
else if test -f /etc/inputrc
  set -x INPUTRC /etc/inputrc
end

#------------------------------------------------------------------------------
# Terminal definition
#------------------------------------------------------------------------------
if test -z "$TERM"
    set -x TERM linux
end

if test "$RUNNING_DARWIN" = 'true'
    if test -n "$TERM_PROGRAM" -a "$TERM_PROGRAM" = 'iTerm.app'
        echo "Running in $TERM_PROGRAM $TERM_PROGRAM_VERSION"
    end

    if test "$TERM" = 'xterm'
        set -x TERM xterm-color
    end
end

#------------------------------------------------------------------------------
# Setup the LS_COLORS environment variable
#------------------------------------------------------------------------------
set -x CLICOLOR ON
set -x LSCOLORS ExFxCxDxBxegedabagacad

#------------------------------------------------------------------------------
# Set LOCATE_PATH - A colon separated list of database file names by the
# locate command.
#------------------------------------------------------------------------------
if test "$RUNNING_CYGWIN" = 'true'
    set -x LOCATE_PATH /usr/var/locatedb
end

#------------------------------------------------------------------------------
# Set CDPATH - A colon separated list of directories used as a
# search path for 'cd'
#------------------------------------------------------------------------------
set CDPATH '.'
set CDPATH $CDPATH "$HOME"

if test -d $HOME/dev/projects
  set CDPATH $CDPATH "$HOME/dev/projects"
end
set -x CDPATH $CDPATH

#---------------------------------------------------------------------------
# Set CPATH - A colon separated list of directories searched
# for include files.
#---------------------------------------------------------------------------
set CPATH '/usr/include'

if test -d /opt/local/include
    set CPATH "/opt/local/include:$CPATH"
end

if test -d /usr/local/include
    set CPATH "/usr/local/include:$CPATH"
end
set -x CPATH $CPATH

#---------------------------------------------------------------------------
# Set LIBRARY_PATH - A colon separated list of directories searched
# for libraries.
#---------------------------------------------------------------------------
set -e LIBRARY_PATH
set sep ''
if test -d /lib
    set LIBRARY_PATH "$sep$LIBRARY_PATH"
    set sep ':'
end

if test -d /usr/lib
    set LIBRARY_PATH "/usr/lib$sep$LIBRARY_PATH"
    set sep ':'
end

if test -d /opt/local/lib
    set LIBRARY_PATH "/opt/local/lib$sep$LIBRARY_PATH"
    set sep ':'
end

if test -d /usr/local/lib
    set LIBRARY_PATH "/usr/local/lib$sep$LIBRARY_PATH"
    set sep ':'
end

set -x LIBRARY_PATH "$LIBRARY_PATH"
set -e sep

#------------------------------------------------------------------------------
# Set the default pager
#------------------------------------------------------------------------------
if test -x /usr/bin/less
    set -x PAGER less

    ###########################################################################
    #
    # Set environment variables for less
    #
    ###########################################################################
    #--------------------------------------------------------------------------
    # Set the options for less
    #
    #   -C : Scroll by clearing and repainting
    #   -e : Exit at end of file (second time)
    #   -g : Highlight only the string found by last search
    #   -i : Ignore case in searches
    #   -n : Suppress line numbers  
    #   -m : Prompt style (Like more)
    #   -Q : Quiet, do not ring the bell for errors
    #   -s : Squeeze multiple blank lines into one
    #   -x : Set tab stops
    #--------------------------------------------------------------------------
    set -x LESS '-eisnmQRs -x4'
else
    set -x PAGER more
end

if test -x /usr/bin/more
  set -x MORE '-erX'
end

#------------------------------------------------------------------------------
# Set the default editor
#------------------------------------------------------------------------------
set -x EDITOR vim
set -x VISUAL vim

#---------------------------------------------------------------------------
# Set the location of the artistic style option file
#---------------------------------------------------------------------------
if test -f "$HOME/config/astylerc"
    set -x ARTISTIC_STYLE_OPTIONS "$HOME/config/astylerc"
end

#---------------------------------------------------------------------------
# Set XNLSPATH - X Locales
#---------------------------------------------------------------------------
if test -d /usr/X11R6/lib/X11/locale
    set -x XNLSPATH /usr/X11R6/lib/X11/locale
else if test -d /usr/X11R6/lib/X11/nls
    set -x  XNLSPATH /usr/X11R6/lib/X11/nls
end

#------------------------------------------------------------------------------
# Set the sgml catalog and xml catalog
#------------------------------------------------------------------------------
set -e sgml
set -e xml

set all_sgml_catalogs '/etc/sgml/catalog'
set all_sgml_catalogs $all_sgml_catalogs '/usr/local/etc/sgml/catalog'
set all_sgml_catalogs $all_sgml_catalogs '/usr/local/share/sgml/catalog'`
set all_sgml_catalogs $all_sgml_catalogs '/usr/share/openjade/catalog'
set all_sgml_catalogs $all_sgml_catalogs '/usr/share/xml/docbook/4.3/docbook.cat'
set all_sgml_catalogs $all_sgml_catalogs '/usr/local/share/docbook-dsssl/catalog'

set all_opensp_dirs '/usr/share/OpenSP'
set all_opensp_dirs $all_opensp_dirs '/opt/local/share/OpenSP'
set sep ''

for cat in $all_sgml_catalogs
    if test -f "$cat"
        set sgml "$sgml$sep$cat"
        set sep ':'
    end
end

set sep ''
for opensp in $all_opensp_dirs
    if test -d "$opensp"
        for cat in (ls $opensp/*.soc)
            set sgml "$sgml$sep$cat"
            set sep ':'
        end
    end
end

set -x SGML_CATALOG_FILES "$sgml"
set -e sgml
set -e cat
set -e all_sgml_catalogs
set -e all_opensp_dirs

set all_xml_catalogs '/etc/xml/catalog'
set all_xml_catalogs $all_xml_catalogs '/opt/local/etc/xml/catalog'
set all_xml_catalogs $all_xml_catalogs '/usr/local/lib/xml/catalog'
set sep ''

for cat in $all_xml_catalogs
    if test -f "$cat"
        set xml "$xml$sep$cat"
        set sep ' '
    end
end
set -x XML_CATALOG_FILES "$xml"
set -e xml
set -e cat
set -e all_xml_catalogs

#------------------------------------------------------------------------------
# Set up html tidy
#------------------------------------------------------------------------------
set -x HTML_TIDY "$HOME/config/tidy"

#------------------------------------------------------------------------------
# Set the EMACSHOME and EMACSVERSION environment variables.
#------------------------------------------------------------------------------
# if [ "$RUNNING_CYGWIN"  = 'true'
# then
#     all_emacsen="\
# 22.3
# 23.3
# 24.1
# 24.2
# 24.3
# 25.2.1"

#     for emacs_version in $all_emacsen
#     do
#       if [ "$RUNNING_CYGWIN" = 'true' ]
#       then 
#         if [ -d "/usr/local/emacs-${emacs_version}" ]
#         then
#           EMACSHOME="/usr/local/emacs-${emacs_version}"
#         fi
#     done
#     unset all_emacsen emacs_version

#     if [ -n "$EMACSHOME" ]
#     then
#       export EMACSHOME
#     fi
# fi

#------------------------------------------------------------------------------
# Set the MANPATH environment variable.
# A colon separated list of directories in which to search for man files
#------------------------------------------------------------------------------
if test -d /opt/local/share/man
    set -x MANPATH "$MANPATH:/opt/local/share/man"
end

#------------------------------------------------------------------------------
# Set the INFOPATH environment variable.
# A colon separated list of directories in which to search for info 'dir' files
#------------------------------------------------------------------------------
set infopath .

set all_info_paths '/opt/local/share/info'
set all_info_paths $all_info_paths '/usr/local/share/info'
set all_info_paths $all_info_paths '/usr/local/info'
set all_info_paths $all_info_paths '/usr/share/info'
set all_info_paths $all_info_paths '/usr/info'
set all_info_paths $all_info_paths '/Applications/MacPorts/Emacs.app/Contents/Resources/info'
set sep ''

for i in $all_info_paths
  if test -d "$i"
    set infopath "$infopath$sep$i"
    set sep ':'
  end
end
set -e all_info_paths

if test -n $EMACSHOME
  if test -f "$EMACSHOME/info/dir"
    set infopath "$infopath$sep$EMACSHOME/info"
  end
end
set -x INFOPATH "$infopath"
set -e infopath
set -e i
set -e sep

#-----------------------------------------------------------------------------
# Fortune
#-----------------------------------------------------------------------------
if test "$RUNNING_FREEBSD" = 'true'
    if test -x /usr/games/fortune
        /usr/games/fortune freebsd-tips
    end
end

#------------------------------------------------------------------------------
# Start in the users home directory
#------------------------------------------------------------------------------
if test ! "$HOME" = "$PWD"
    cd "$HOME"
end

#------------------------------------------------------------------------------
# Setup ssh-agent
#------------------------------------------------------------------------------
if test "$RUNNING_CYGWIN" = 'true'
    if test -x "$HOME/bin/sshtool"
        eval ("$HOME/bin/sshtool --fish")
    end

    echo 'Loading ssh keys ...'

    for key in id_rsa id_dsa trexone_rsa
        if test -e "$HOME/.ssh/$key"
            echo "$key"
            keychain --quick "$HOME/.ssh/$key"
            . "$HOME/.keychain/$HOSTNAME-sh" > /dev/null
        end
    end
    set -e key
end

#---------------------------------------------------------------------------
alias path='echo $PATH'
alias cls='clear'

#------------------------------------------------------------------------------
# Alias shortcuts for ls
#
# Linux and Cygwin use GNU ls which has --color=auto
# darwin, BSDs, and Sun do not
#------------------------------------------------------------------------------
if test "$RUNNING_DARWIN" = 'true' -o "$RUNNING_FREEBSD" = 'true'
    alias ls='ls -hF'
else
    alias ls='ls -hF --color=auto'    # Use colors
end

alias l='ls -CF'
alias ll='ls -l'
alias lsc='ls -adl'                   # ls config files
alias lsd='ls -dl'                    # ls directory

# Interactive operation...
# alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

alias du='du -h'
alias df='df -h'
alias hup='kill -HUP'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias p='popd'

if test "$RUNNING_DARWIN" = 'true' -o "$RUNNING_FREEBSD" = 'true'
    alias top='top -o cpu'
end

alias ed='ed -p\*'

#---------------------------------------------------------------------------
# c -  Change directory function
#---------------------------------------------------------------------------
function c
    if test "$argv[1]" = '--'
        dirs
        return 0
    else if test "$argv[1]" = '=='
        cdh
        return 0
    end

    set the_new_dir $argv[1]
    if test -z "$the_new_dir"
        set the_new_dir $HOME
    end

    set -l teststr (string sub -s 1 -l 2 $the_new_dir)
    if test "$teststr" = '~'
        set -l the_rest (string sub -s 2)
        set the_new_dir "$HOME$the_rest" 
    end
    
    pushd "$the_new_dir" > /dev/null
    if test $status -ne 0
        return 1
    end
    set -e the_new_dir
end

#---------------------------------------------------------------------------
# emacs - Start the emacs chainsaw er editor
#---------------------------------------------------------------------------
function emacs
    set -l prg

    if test "$RUNNING_CYGWIN" = 'true' -o "$RUNNING_MSYS2_BASED_ENV" = 'true'
        if test -x /usr/bin/emacs
            set prg 'emacs'
        else if test -n "$EMACSHOME"
            set prg "$EMACSHOME/bin/runemacs"
        else if test -x /mingw32/bin/emacs
            set prg /mingw32/bin/emacs
        else if test -x /mingww64/bin/emacs
            set prg /mingw64/bin/emacs
        end
    else if test "$RUNNING_DARWIN" = 'true'
        set prg '/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs'
    else
        set prg 'emacs'
    end

    eval "command $prg -g 80x50 $argv"
end

#---------------------------------------------------------------------------
# lines - Print the number of lines in file(s)
#---------------------------------------------------------------------------
function lines
    wc -l $argv
end

###############################################################################
# End of config.fish
###############################################################################

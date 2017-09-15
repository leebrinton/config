"=============================================================================
"
" vimrc
"
" vim - VI iMproved configuration file
"
"==============================================================================
" debug
"set verbose=9
" Settings used by vi, and other vi alikes

if has( "gui_win32" )
  source $VIM/exrc
else
  source ~/config/exrc
endif

" Avoid warning for wrong version
version 5.0

"------------------------------------------------------------------------------
" Important options
"------------------------------------------------------------------------------

" Act like Vim not Vi
set nocompatible

" Vi compatible options
" a - Set alternate filename on :read
" A - Set alternate filename on :write
"              b - \| in a ":map" command is the end of the map command
" B - A backslash has no special meaning in mappings
" c - Start searching for the next match
"     one character after the current match
" C - Do not concatinate sourced
"     lines that start with a backslash
" d - "./" in the tags option means use the tags
"     file in thecurrent directory,
"     not the tags file reletive to the current file
" D - Can't use CTRL-K to enter a digraph after Normal mode
"     commands with a character argument, like r, f and t.
" e - When executing a register with ":@r",
"     always add a <CR> to the last line
" E - It is an error when using "y", "d", "c", "g~", "gu" or
"     "gU" on an Empty region.
" f - When included, a ":read" command with a file name
"     argument will set the file name for the current buffer,
"     if the current buffer doesn't have a file name yet.
" F - A :write command with a filename
"     will set the file name for the current buffer
" i - When included, interrupting the reading of a file will
"     leave it modified.
" j - When joining lines, only add two spaces after a '.',
"     not after '!' or '?'.  Also see 'joinspaces'.
" k - Disable the recognition of raw key codes in
"     mappings, abbreviations, and the "to" part of menu
"     commands.  For example, if <Key> sends ^[OA (where ^[
"     is <Esc>), the command ":map X ^[OA" results in X
"     being mapped to:
"       'k' included:   "^[OA"   (3 characters)
"       'k' excluded:   "<Key>"  (one key code)
"     Also see the '<' flag below.
" K - Don't wait for a key code to complete when it is
"     halfway a mapping.  This breaks mapping <F1><F1> when
"     only part of the second <F1> has been read.  It
"     enables cancelling the mapping by typing <F1><Esc>.
" l - Backslash in a [] range in a search pattern is taken
"     literally, only "\]" is special  See |/[]|
"       'l' included: "/[ \t]"  finds <Space>, '\' and 't'
"       'l' excluded: "/[ \t]"  finds <Space> and <Tab>
" L - When the 'list' option is set, 'wrapmargin',
"     'textwidth' and Virtual replace mode (see |gR|) count
"     a <Tab> as two characters, instead of the normal
"     behavior of a <Tab>.
" m - When included, a showmatch will always wait half a
"     second.  When not included, a showmatch will wait half
"     a second or until a character is typed.
" n - When included, the column used for 'number' will also
"     be used for text of wrapped lines.
" o - Line offset to search command is not remembered for
"     next search.
" O - Don't complain if a file is being overwritten, even
"     when it didn't exist when editing it.  This is a
"     protection against a file unexpectedly created by
"     someone else.  Vi didn't complain about this.
" p - Vi compatible Lisp indenting.  When not present, a
"     slightly better algorithm is used.
" r - Redo ("." command) uses "/" to repeat a search
"     command, instead of the actually used search string.
" s - Set buffer options when entering the
"     buffer for the first time
" S - Set buffer options always when entering a buffer
"     (except 'readonly', 'fileformat', 'filetype' and
"     'syntax').  This is the (most) Vi compatible setting.
"     The options are set to the values in the current
"     buffer.  When you change an option and go to another
"     buffer, the value is copied.  Effectively makes the
"     buffer options global to all buffers.
"       's'    'S'     copy buffer options
"       no     no      when buffer created
"       yes    no      when buffer first entered (default)
"        X     yes     each time when buffer entered (vi comp.)
" t - Search pattern for the tag command is remembered for
"     "n" command.  Otherwise Vim only puts the pattern in
"     the history for search pattern, but doesn't change the
"     last used search pattern.
" u - Undo is Vi compatible.  See |undo-two-ways|.
" w - When using "cw" on a blank character, only change one
"     character and not all blanks until the start of the
"     next word.
" W - Don't overwrite a readonly file.  When omitted, ":w!"
"     overwrites a readonly file, if possible.
" x - <Esc> on the command-line executes the command-line.
"     The default in Vim is to abandon the command-line,
"     because <Esc> normally aborts a command.  c_<Esc>
" y - A yank command can be redone with ".".
" ! - When redoing a filter command, use the last used
"     external command, whatever it was.  Otherwise the last
"     used -filter- command is used.
" $ - When making a change to one line, don't redisplay the
"     line, but put a '$' at the end of the changed text.
"     The changed text will be overwritten when you type the
"     new text.  The line is redisplayed if you type any
"     command that moves the cursor from the insertion
"     point.
" % - Vi-compatible matching is done for the "%" command.
"     Does not recognize "#if", "#endif", etc.
"     Does not recognize "/*" and "*/".
"     Parens inside single and double quotes are also
"     counted, causing a string that contains a paren to
"     disturb the matching.  For example, in a line like
"     "if (strcmp("foo(", s))" the first paren does not
"     match the last one.  When this flag is not included,
"     parens inside single and double quotes are treated
"     specially.  When matching a paren outside of quotes,
"     everything inside quotes is ignored.  When matching a
"     paren inside quotes, it will find the matching one (if
"     there is one).  This works very well for C programs.
" * - Use ":*" in the same way as ":@".  When not included,
"     ":*" is an alias for ":'<,'>", select the Visual area.
" < - Disable the recognition of special key codes in <>
"     form in mappings, abbreviations, and the "to" part of
"     menu commands.  For example, the command
"     ":map X <Tab>" results in X being mapped to:
"       '<' included:   "<Tab>"  (5 characters)
"       '<' excluded:   "^I"     (^I is a real <Tab>)
"     Also see the 'k' flag above.
set cpoptions=aABceFs

" Makes Vim work in a way that Insert mode is the default mode. 
set noinsertmode

" autoindent when you paste code. pastetoggle to turn off autoindent
set nopaste
set pastetoggle=<F2>

" use defaults for runtimepath and helpfile
"set runtimepath
"set helpfile

"------------------------------------------------------------------------------
" Moving around, searching and patterns
"------------------------------------------------------------------------------
" Allow specified keys that move the cursor left/right to wrap to the
" previous/next line when the cursor is on the first/last character in
" the line.  Concatenate characters to allow this for these keys:
"    char   key	   mode ~
"     b    <BS>	   Normal and Visual
"     s    <Space> Normal and Visual
"     h    "h"     Normal and Visual
"     l    "l"     Normal and Visual
"     <    <Left>  Normal and Visual
"     >    <Right> Normal and Visual
"     ~    "~"     Normal
"     [    <lEFT>  Insert and Replace
"     ]    <Right> Insert and Replace
set whichwrap=b,s,<,>,~,[,]

" Many jump commands move the cursor to the first non-blank
" character of a line
set startofline

" nroff macro names that separate paragraphs
set paragraphs=IPLPPPQPP\ LIpplpipbp

" nroff macro names that separate sections
" set in .exrc
"set sections=

" Comma separated list of directories to search for files
if has( 'unix' )
    " .  the directory of the current file
    " ,, the current directory
    " ~  the user home direcory
    set path=.,,~,/usr/include,/usr/local/include,$VIMRUNTIME/syntax
else
    set path=.,,$VIM\\syntax
endif

" list of directory names used for :cd
"set cdpath=

" search commands wrap around the end of the buffer
" this is set in .exrc
"set wrapscan

" incremental searching
set incsearch

" Allow unescaped * and . in search patterns
" this is set in .exrc
" set magic

" Don't ignore case in search patterns
set noignorecase

" override 'ignorecase' when pattern has upper case characters
set smartcase

" pattern for a macro definition line
" default is for C programs 
"set define=

" pattern for an include-file line
" default is for C programs
"set include=

" expression used to transform an include line to a file name
" local to buffer so we don't set it here
"set includeexpr=

"------------------------------------------------------------------------------
" tags
"------------------------------------------------------------------------------
" use binary searching in tags files
set tagbsearch

" number of significant characters in a tag name or zero
" set in .exrc
"set taglength=0

" list of file names to search for tags
" set in .exrc
"set tags=

" file names in a tags file are relative to the tags file
set tagrelative

" a :tag command will use the tagstack
set tagstack

" when completing tags in Insert mode show more info
set showfulltag

"------------------------------------------------------------------------------
" displaying text
"------------------------------------------------------------------------------
" number of lines to scroll for CTRL-U and CTRL-D
"set in .exrc
"set scroll=

" number of screen lines to show around the cursor
set scrolloff=0

" long lines wrap
set wrap

" wrap long lines at a character in 'breakat'
set nolinebreak

" which characters might cause a line break
"set breakat=

" string to put before wrapped screen lines
set showbreak=>

" minimal number of columns to scroll horizontally
set sidescroll=0

" minimal number of columns to keep left and right of the cursor
set sidescrolloff=0

" include "lastline" to show the last line even if it doesn't fit
" include "uhex" to show unprintable characters as a hex number
set display=uhex

" characters to use for the status line, folds and filler lines
"set fillchars=

" number of lines used for the command-line
set cmdheight=1

" width of the display
set columns=80

if has( 'gui_running' )
    set lines=57
endif

" don't redraw while executing macros
set nolazyredraw

" delay in msec for each char written to the display
" (for debugging)
set writedelay=0

" show <Tab> as ^I and end-of-line as $
" set in .exrc
"set nolist

" list of strings used for list mode
set listchars=eol:$,tab:T#,trail:_,extends:\\,precedes:<

" show the line number for each line
set nonumber

"------------------------------------------------------------------------------
" syntax and highlighting
"------------------------------------------------------------------------------

" Use colors that look good on our background
if has( 'gui_running' )
    set background=light
"else
"    set background=dark
endif

"filetype on

" Use syntax coloring
if has( 'syntax' )
    syntax on

    if has( 'gui_running' )
        if filereadable( expand( "~/.vim/colors/solarized.vim" ))
            colorscheme solarized
        endif
    endif
endif

if &term=="xterm"
	set t_RV=          " don't check terminal version
	set t_Co=8
	set t_Sb=^[4%dm
	set t_Sf=^[3%dm
endif

" which highlighting to use for various occasions
"set highlight

if has("win32")
    highlight Comment    term=bold      ctermfg=Cyan
    highlight Constant   term=underline ctermfg=White
    highlight Special    term=bold      ctermfg=Red
    highlight Identifier term=underline ctermfg=Grey
    highlight Statement  term=bold      ctermfg=Yellow
    highlight PreProc    term=underline ctermfg=Magenta
    highlight Type       term=underline ctermfg=Green
elseif &term=="cygwin"
    highlight Comment    term=bold      ctermfg=Cyan
    highlight Constant   term=underline ctermfg=White
    highlight Special    term=bold      ctermfg=Red
    highlight Identifier term=underline ctermfg=Grey
    highlight Statement  term=bold      ctermfg=Yellow
    highlight PreProc    term=underline ctermfg=Magenta
    highlight Type       term=underline ctermfg=Green
"else
"   highlight Comment    term=bold      ctermfg=DarkCyan
"   highlight Constant   term=underline ctermfg=Grey
"   highlight Special    term=bold      ctermfg=DarkRed
"   highlight Identifier term=underline ctermfg=Grey
"   highlight Statement  term=bold      ctermfg=Brown
"   highlight PreProc    term=underline ctermfg=DarkGreen
"   highlight Type       term=underline ctermfg=DarkGreen
endif

" highlight all matches for the last used search pattern
set hlsearch

"------------------------------------------------------------------------------
" multiple windows
"------------------------------------------------------------------------------

" Always show a status line
set laststatus=2

" format to be used for a status line
"set statusline=
set statusline=
set statusline+=%3.3n\                       " buffer number
set statusline+=%f\                          " filename
set statusline+=%h%m%r%w                     " status flags
set statusline+=\[%{strlen(&ft)?&ft:'none'}] "file type
set statusline+=%=                           " right align remainder
set statusline+=0x%-8B                       " character value
set statusline+=%-14(%l,%c%V%)                " line, character
set statusline+=%<%P                         " file position

" make all windows the same size when adding/removing windows
set equalalways

" in which direction 'equalalways' works: "ver", "hor" or "both"
set eadirection=both

" minimal number of lines used for the current window
set winheight=1

" minimal number of lines used for any window
set winminheight=1

" minimal number of columns used for the current window
set winwidth=20

" minimal number of columns used for any window
set winminwidth=1

" initial height of the help window
set helpheight=20

" default height for the preview window
set previewheight=12

" identifies the preview window
" (local to window)
set nopreviewwindow

" don't unload a buffer when no longer shown in a window
set hidden

" "useopen" and/or "split"; which window to use when jumping
" to a buffer
set switchbuf=split

" a new window is put below the current one
set splitbelow

" a new window is put right of the current one
set splitright

" this window scrolls together with other bound windows
" (local to window)
"set noscrollbind

" "ver", "hor" and/or "jump"; list of options for 'scrollbind'
set scrollopt=ver,hor,jump

"------------------------------------------------------------------------------
" terminal
"------------------------------------------------------------------------------

" name of the used terminal
" Best to set the environment variable $TERM
" set in .exrc
"set term=

" alias for 'term': see term
"set ttytypr=

" check built-in termcaps first
set ttybuiltin

" terminal connection is fast
set ttyfast

" terminal that requires extra redrawing
set noweirdinvert

" Allow cursor keys in insert mode
set esckeys

" Number of lines to scroll when the cursor get off the screen
set scrolljump=1

" maximum number of lines to use scrolling instead of redrawing
" set in .exrc
"set ttyscroll=999

" specifies what the cursor looks like in different modes
"set guicursor=

" show info in the window title
set title

" percentage of 'columns' used for the window title
set titlelen=85

" when not empty, string to be used for the window title
"set titlestring

" string to restore the title to when exiting Vim
set titleold=Thanks\ for\ flying\ Vim

" set the text of the icon for this window
set icon

" when not empty, text for the icon of this window
set iconstring=Vim\ %F

" restore the screen contents when exiting Vim
set restorescreen

"------------------------------------------------------------------------------
" using the mouse
"------------------------------------------------------------------------------

if has("mouse")
   " list of flags for using the mouse
   set mouse=a

   " the window with the mouse pointer becomes the current one
   set nomousefocus

   " hide the mouse pointer while typing
   set mousehide

   " "extend", "popup" or "popup_setpos"; what the right
   " mouse button is used for
   set mousemodel=popup

   " maximum time in msec to recognize a double-click
   set mousetime=500

   " "xterm", "xterm2", "dec" or "netterm"; type of mouse
   "set ttymouse=

   " what the mouse pointer looks like in different modes
   "set mouseshape=
endif

"------------------------------------------------------------------------------
" GUI
"------------------------------------------------------------------------------

if has("gui_win32")
    set guifont=Courier_New:h9
endif

" list of font names to be used for double-wide characters
"set guifontwide=

" list of flags that specify how the GUI works
set guioptions=agmrLT

" use a pseudo-tty for I/O to external commands
set guipty

" "last", "buffer" or "current": which directory used for the file browser
set browsedir=buffer

" language to be used for the menus
"set langmenu=

" maximum number of items in one menu
set menuitems=25

" "no", "yes" or "menu"; how to use the ALT key
"set winaltkeys=

" number of pixel lines to use between characters
"set linespace=

"------------------------------------------------------------------------------
" printing
"------------------------------------------------------------------------------
" name of the printer to be used for :hardcopy
if has( 'win32' )
    set printdevice=//Apollo/Dev
endif

" expression used to print the PostScript file for :hardcopy
"set printexpr=

" name of the font to be used for :hardcopy
set printfont=Courier_New:h9

" format of the header used for :hardcopy
"set printheader=

" list of items that control the format of :hardcopy output
set printoptions=paper:letter

"------------------------------------------------------------------------------
" messages and info
"------------------------------------------------------------------------------

" add 's' flag in 'shortmess' (don't show search message)
set noterse

" list of flags to make messages shorter
set shortmess=filnrxoO

" show (partial) command keys in the status line
set showcmd

" display the current mode in the status line
" set in .exrc
"set showmode

" show cursor position below each window
set ruler

" alternate format to be used for the ruler
"set rulerformat=

" threshold for reporting number of changed lines
" set in .exrc
"set report=2

" the higher the more messages are given
set verbose=0

" pause listings when the screen is full
set more

" start a dialog when a command fails
set noconfirm

" ring the bell for error messages
" set in .exrc
"set noerrorbells

" use a visual bell instead of beeping
set visualbell

"------------------------------------------------------------------------------
" selecting text
"------------------------------------------------------------------------------

" "old", "inclusive" or "exclusive"; how selecting text behaves
set selection=exclusive

" "mouse", "key" and/or "cmd"; when to start Select mode
"  instead of Visual mode
"set selectmode=

" "unnamed" to use the * register like unnamed register
set clipboard=autoselect

" "startsel" and/or "stopsel"; what special keys can do
"set keymodel=

"------------------------------------------------------------------------------
" editing text
"------------------------------------------------------------------------------

" maximum number of changes that can be undone
set undolevels=500

" changes have been made and not written to a file
" (local to buffer)
"set modified

" buffer is not to be written
" (local to buffer)
"set readonly

" changes to the text are not possible
" (local to buffer)
"set modifiable

" line length above which to break a line
" (local to buffer)
set textwidth=78

" margin from the right in which to break a line
" (local to buffer)
" set in .exrc
"set wrapmargin

" specifies what <BS>, CTRL-W, etc. can do in Insert mode
" Allow backspacing over everything in insert mode
set backspace=2

" definition of what comment lines look like
" (local to buffer)
"set comments=

" list of flags that tell how automatic formatting works
" (local to buffer)
" Options for the textformat command
" t Auto-wrap text using textwidth
" c Auto-wrap comments using textwidth, inserting the current comment leader
" r Automatically insert the current comment leader after hitting
"   return in insert mode.
" o Automatically insert the current comment leader after hitting 'o' or 'O'
"   in normal mode.
" q Allow formatting of comments with "gg".
" 2 When formatting text, use the indent the second line of a paragraph
"   for the rest of the paragraph.
" v Vi-compatible auto-wrapping in insert mode: Only break a line at a blank
"   that you have entered during the current insert command.
" b Like 'v', but only auto-wrap if you enter a blank at or before the wrap
"   margin.
" l Long lines are not broken in insert mode: When a line was longer than
"   'textwidth' when the insert command started, Vim does not automatically
"   format it.
set formatoptions=cqr

" specifies how Insert mode completion works
" (local to buffer)
"   . - scan the current buffer
"   w - scan the other windows
"   b - scan other loaded buffers
"   t - tag completion
set complete=.,w,b,t,i,k

"list of dictionary files for keyword completion
if has( "unix" )
    if filereadable( "/usr/dict/words" )
        set dictionary=/usr/dict/words
    endif
else
    if filereadable( "c:/cygwin/usr/dict/words" )
        set dictionary=c:/cygwin/usr/dict/words
    endif
endif

" list of thesaurus files for keyword completion
"set thesaurus=

" adjust case of a keyword completion match
set noinfercase

" enable entering digraps with c1 <BS> c2
set nodigraph

" the "~" command behaves like an operator
set notildeop

" When inserting a bracket, briefly jump to its match
" set in .exrc
"set showmatch

" tenth of a second to show a match for 'showmatch'
set matchtime=7

" list of pairs that match for the "%" command
" (local to buffer)
set matchpairs=(:),{:},[:]

" use two spaces after '.' when joining a line
" Insert two spaces after a period when joining lines
set joinspaces

" "alpha", "octal" and/or "hex"; number formats recognized for
" CTRL-A and CTRL-X commands
" (local to buffer)
set nrformats=alpha,octal,hex

"------------------------------------------------------------------------------
" tabs and indenting
"------------------------------------------------------------------------------

" number of spaces a <Tab> in the text stands for
" set in .exrc
"set tabstop=8

" number of spaces used for each step of (auto)indent
" (local to buffer)
" set in .exrc
"set shiftwidth=4

" a <Tab> in an indent inserts 'shiftwidth' spaces
set smarttab

" if non-zero, number of spaces to insert for a <Tab>
" (local to buffer)
set softtabstop=4

" round to 'shiftwidth' for "<<" and ">>"
set shiftround

" expand <Tab> to spaces in Insert mode
" (local to buffer)
set expandtab

" automatically set the indent of a new line
" (local to buffer)
" initally set in .exrc
"set noautoindent

" do clever autoindenting
" (local to buffer)
set nosmartindent

" enable specific indenting for C code
" (local to buffer)
set nocindent

" options for C-indenting
" (local to buffer)
set cinoptions=

" keys that trigger C-indenting in Insert mode
" (local to buffer)
set cinkeys=

" list of words that cause more C-indent
" (local to buffer)
set cinwords=

" expression used to obtain the indent of a line
" (local to buffer)
set indentexpr=

" keys that trigger indenting with 'indentexpr' in Insert mode
" (local to buffer)
set indentkeys=

" enable lisp mode
" (local to buffer)
" initally set in .exrc
"set nolisp

" words that change how lisp indenting works
"set lispwords

"------------------------------------------------------------------------------
" folding
"------------------------------------------------------------------------------
if has("folding")
   " set to display all folds open
   " (local to window)
   "set foldenable

   " folds with a level higher than this number will be closed
   " (local to window)
   set foldlevel=0

   " value for 'foldlevel' when starting to edit a file
   set foldlevelstart=-1

   " width of the column used to indicate folds
   " (local to window)
   set foldcolumn=0

   " expression used to display the text of a closed fold
   " (local to window)
   set foldtext=foldtext()

   " set to "all" to close a fold when the cursor leaves it
   set foldclose=

   " specifies for which commands a fold will be opened
   set foldopen=block,hor,mark,percent,quickfix,search,tag,undo

   " minimum number of screen lines for a fold to be closed
   " (local to window)
   set foldminlines=1

   " template for comments; used to put the marker in
   set commentstring=/*%s*/

   " folding type: "manual", "indent", "expr", "marker" or "syntax"
   " (local to window)
   set foldmethod=manual

   " expression used when 'foldmethod' is "expr"
   " (local to window)
   set foldexpr=0

   " used to ignore lines when 'foldmethod' is "indent"
   " (local to window)
   set foldignore=#

   " markers used when 'foldmethod' is "marker"
   " (local to window)
   set foldmarker={{{,}}}

   " maximum fold depth for when 'foldmethod is "indent" or "syntax"
   " (local to window)
   set foldnestmax=20
endif

"------------------------------------------------------------------------------
" diff mode
"------------------------------------------------------------------------------
" use diff mode for the current window
" (local to window)
set nodiff

" options for using diff mode
set diffopt=filler

" expression used to obtain a diff file
set diffexpr=

" expression used to patch a file
set patchexpr=

"------------------------------------------------------------------------------
" mapping
"------------------------------------------------------------------------------
" maximum depth of mapping
set maxmapdepth=1000

" recognize mappings in mapped keys
" set in .exrc
"set remap

" allow timing out halfway into a mapping
" set in .exrc
"set to

" allow timing out halfway into a key code
set nottimeout

" time in msec for 'timeout'
set timeoutlen=1000

" time in msec for 'ttimeout'
set ttimeoutlen=-1

"------------------------------------------------------------------------------
" reading and writing files
"------------------------------------------------------------------------------

" enable using settings from modelines when reading a file
" (local to buffer)
set modeline
    
" number of lines to check for modelines
set modelines=5
    
" binary file editing
" (local to buffer)
set nobinary
    
" last line in the file has an end-of-line
" (local to buffer)
set endofline

" Prepend a Byte Order Mark to the file
" (local to buffer)
set nobomb

" end-of-line format: "dos", "unix" or "mac"
" (local to buffer)
"set fileformat=

" list of file formats to look for when editing a file
if has( "unix" )
    set fileformats=unix,dos
else
    set fileformats=dos,unix
endif

" obsolete, use 'fileformat'
" (local to buffer)
set notextmode

" obsolete, use 'fileformats'
set textauto
    
" writing files is allowed
set write

" write a backup file before overwriting a file
set writebackup

" keep a backup after overwriting a file
set backup
    
" patterns that specify for which files a backup is not made
set backupskip=

" wether to make the backup as a copy or rename the existing file
set backupcopy=auto

 
" list of directories to put backup files in
" use defaults
"if has( "unix" )
"    set backupdir=$HOME/.backups
"else
"    set backupdir=C:\TEMP
"endif

" file name extension for the backup file
set backupext=~

" automatically write a file when leaving a modified buffer
" set in .exrc
"set noautowrite
    
" as 'autowrite', but works with more commands
set noautowriteall

" always write without asking for confirmation
" set in .exrc
" set nowriteany

" automatically read a file when it was modified outside of Vim
" (global or local to buffer)
set noautoread

" keep oldest version of a file; specifies file name extension
set patchmode=

" use 8.3 file names
" (local to buffer)
set noshortname

"------------------------------------------------------------------------------
" the swap file
"------------------------------------------------------------------------------
" directory	list of directories for the swap file
"set dir=

" use a swap file for this buffer
" (local to buffer)
set swapfile

" "sync", "fsync" or empty; how to flush a swap file to disk
set swapsync=fsync

" number of characters typed to cause a swap file update
set updatecount=200

" time in msec after which the swap file will be updated
set updatetime=4000

" maximum amount of memory in Kbyte used for one buffer
set maxmem=280448

" maximum amount of memory in Kbyte used for all buffers
set maxmemtot=280448

"------------------------------------------------------------------------------
" command line editing
"------------------------------------------------------------------------------

" how many command lines are remembered 
set history=20

" key that triggers command-line expansion
set wildchar=9

" like 'wildchar' but can also be used in a mapping
set wildcharm=0

" specifies how command line completion works
set wildmode=full

" list of file name extensions that have a lower priority
set suffixes=.bak,~,.o,.h,.info,.swp,.class

" list of file name extensions added when searching for a file
" (local to buffer)
set suffixesadd=

" list of patterns to ignore files for file name completion
set wildignore=
    
" command-line completion shows a list of matches
set wildmenu

" key used to open the command-line window
set cedit=
    
" height of the command-line window
set cmdwinheight=7

"------------------------------------------------------------------------------
" executing external commands
"------------------------------------------------------------------------------

" name of the shell program used for external commands
if has( 'unix' )
    set shell=/bin/bash
else
    set shell=cmd.exe
endif

" character(s) to enclose a shell command in
"set shellquote=

" like 'shellquote' but include the redirection
"set shellxquote=

" argument for 'shell' to execute a command
"set shellcmdflag=/c

" used to redirect command output to a file
"set shellredir=

" program used for "=" command
" (global or local to buffer)
set equalprg=

" program used to format lines with "gq" command
set formatprg=

" program used for the "K" command
set keywordprg=man

" warn when using a shell command and a buffer has changes
" set in .exrc
"set warn

"------------------------------------------------------------------------------
" running make and jumping to errors
"------------------------------------------------------------------------------

" name of the file that contains error messages
set errorfile=errors

" list of formats for error messages
" (global or local to buffer)
set errorformat=%f(%l)\ :\ %t%*\\D%n:\ %m,%*[^\"]\"%f\"%*\\D%l:\ %m,%f(%l)\ :\ %m,%*[^\ ]\ %f\ %l:\ %m,%f:%l:%m

" program used for the ":make" command
" (global or local to buffer)
set makeprg=make

" string used to put the output of ":make" in the error file
"set shellpipe=

" name of the errorfile for the 'makeprg' command
set makeef=
    
" program used for the ":grep" command
" (global or local to buffer)
"set grepprg=

" list of formats for output of 'grepprg'
set grepformat=%f:%l:%m,%f:%l%m,%f\ \ %l%m

"------------------------------------------------------------------------------
" system specific
"------------------------------------------------------------------------------

" use forward slashes in file names; for Unix-like shells
set noshellslash
"------------------------------------------------------------------------------
" language specific
"------------------------------------------------------------------------------

" specifies the characters in a file name
set isfname=@,48-57,/,\\,.,-,_,+,,,#,$,%,{,},[,],:,@-@,!,~,=

" specifies the characters in an identifier
set isident=@,48-57,_,128-167,224-235

" specifies the characters in a keyword
" (local to buffer)
set iskeyword=@,48-57,_,128-167,224-235

" specifies printable characters
set isprint=@,~-255

" display the buffer right-to-left
" (local to window)
set norightleft

" Insert characters backwards
set norevins

" Allow CTRL-_ in Insert and Command-line mode to toggle 'revins'
set noallowrevins

" the ASCII code for the first letter of the Hebrew alphabet
set aleph=224

" use Hebrew keyboard mapping
set nohkmap

" use phonetic Hebrew keyboard mapping
set nohkmapp

" use Farsi as the second language when 'revins' is set
set noaltkeymap

" use Farsi keyboard mapping
set nofkmap

" name of a keyboard mappping
set keymap=

" translate characters for Normal mode
set langmap=

" in Insert mode: 1: use :lmap; 2: use IM; 0: neither
" (local to window)
set iminsert=0

" entering a search pattern: 1: use :lmap; 2: use IM; 0: neither
" (local to window)
set imsearch=0

"------------------------------------------------------------------------------
" multi-byte characters
"------------------------------------------------------------------------------

" character encoding used in Vim: "latin1", "utf-8"
" "euc-jp", "big5", etc.
set encoding=utf-8

" character encoding for the current file
" (local to buffer)
set fileencoding=

" automatically detected character encodings
set fileencodings=ucs-bom

" character encoding used by the terminal
set termencoding=

" expression used for character encoding conversion
set charconvert=

" Delete combining (composing) characters on their own
set nodelcombine

"------------------------------------------------------------------------------
" various 
"------------------------------------------------------------------------------
" when to use virtual editing: "block", "insert" and/or "all"
set virtualedit=

" list of autocommand events which are to be ignored
set eventignore=

" load plugin scripts when starting up
set loadplugins

" enable reading .vimrc/.exrc/.gvimrc in the current directory
set noexrc

" safer working with script files in the current directory
set nosecure

" use the 'g' flag for ":substitute"
set nogdefault

" 'g' and 'c' flags of ":substitute" toggle
" set in .exrc
"set noedcompatible

" maximum depth of function calls
set maxfuncdepth=100

" list of words that specifies what to put in a session file
set sessionoptions=blank,buffers,curdir,folds,help,options,winsize

" list of words that specifies what to save for :mkview
set viewoptions=folds,options,cursor

" directory where to store files with :mkview
"set viewdir=

" list that specifies what to write in the viminfo file
set viminfo='20,\"50

" what happens with a buffer when it's no longer in a window
" (local to buffer)
set bufhidden=

" "", "nofile", "nowrite" or "quickfix": type of buffer
" (local to buffer)
set buftype=

" whether the buffer shows up in the buffer list
" (local to buffer)
set buflisted

" set to "msg" to see all error messages
set debug=

"------------------------------------------------------------------------------
if has("autocmd")
  augroup gentoo
    au!

    " Gentoo-specific settings for ebuilds.  These are the federally-mandated
    " required tab settings.  See the following for more information:
    " http://www.gentoo.org/doc/en/xml/gentoo-howto.xml
    au BufRead,BufNewFile *.e{build,class} let is_bash=1|setft=sh
    au BufRead,BufNewFile *.e{build,class} set ts=4 sw=4 noexpandtab

    " In text files, limit the width of text to 78 characters, but be careful
    " that we don't override the user's setting.
    autocmd BufNewFile,BufRead *.txt if &tw == 0 | set tw=78 | endif

    " When editing a file, always jump to the last cursor position
    autocmd BufReadPost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
      \   exe "normal g'\"" |
      \ endif
  augroup END

"------------------------------------------------------------------------------
  augroup cprog
    " Remove all cprog autocommands
    autocmd! cprog

    " When starting to edit a file:
    "   For *.c and *.h files set formatting of comments and set C-indenting on
    "   For other files switch it off
    "   Don't change the sequence,
    "   it's important that the line with * comes first.
    autocmd BufRead * set formatoptions=tcql nocindent comments&
    autocmd BufRead *.c,*.h set formatoptions=croql
    autocmd BufRead *.c,*.h set cindent
    autocmd BufRead *.c,*.h set cinoptions=:0,g0,t0,(s
    autocmd BufRead *.c,*.h set comments=sr:/*,mb:*,el:*/,://
    autocmd BufRead *.c,*.h set include="^#\\*include"
    autocmd BufRead *.c,*.h set define="^\\(#\\s*define\\|[a-z]*\\s*const\\s*[a-z]*\\)"
  augroup END

"------------------------------------------------------------------------------
  augroup perlprog
    " Remove all perlprog autocommands
    autocmd! perlprog

    autocmd BufRead *.pl set formatoptions=croql
    autocmd BufRead *.pl set cindent
    autocmd BufRead *.pl set cinoptions=:0,g0,t0,(1s 
  augroup END

"------------------------------------------------------------------------------
  augroup java
    " Remove all java autocommands
    autocmd! java

    autocmd BufRead *.java set formatoptions=croql
    autocmd BufRead *.java set cindent
    autocmd BufRead *.java set comments=sr:/*,mb:*,el:*/,://
    autocmd BufRead *.java set include="^import"
    autocmd BufRead *.java set errorformat=%f:%l:\ %m
  augroup END

"------------------------------------------------------------------------------
  " perl -cw buffer, open output in a new window
  function! PerlCWBuffer()
    let l:tmpfile1 = tempname()
    let l:tmpfile2 = tempname()

    execute "normal:w!" . l:tmpfile1 . "\<CR>"
    execute "normal:! perl -cw ".l:tmpfile1." \> ".l:tmpfile2." 2\>\&1 \<CR>"
    execute "normal:new\<CR>"
    execute "normal:edit " . l:tmpfile2 . "\<CR>"
  endfunction

  " run buffer as a perl script, open output in a new window
  function! PerlBuffer()
    let l:tmpfile1 = tempname()
    let l:tmpfile2 = tempname()
    
    execute "normal:w!" . l:tmpfile1 . "\<CR>"
    execute "normal:! perl ".l:tmpfile1." \> ".l:tmpfile2." 2\>\&1 \<CR>"
    execute "normal:new\<CR>"
    execute "normal:edit " . l:tmpfile2 . "\<CR>"
  endfunction

  " set of crap we run when editing perl source
  function! MyPerlSettings()
    if !did_filetype()
        set filetype=perl
    endif

    set textwidth=78
    set expandtab
    set tabstop=8
    set shiftwidth=4
    set cindent
    set comments=:#
    set formatoptions=croql
    set keywordprg=perldoc\ -f

    noremap <f1> <Esc>:call PerlCWBuffer()<CR><Esc>
    noremap <f2> <Esc>:close<CR><Esc>
    noremap <f3> <Esc>:call PerlBuffer()<CR><Esc>
  endfunction

  " vim won't auto-recognise some of my perl - force an override with f4
  noremap <f4> <esc>:call MyPerlSettings()<CR>

  autocmd FileType perl :call MyPerlSettings()
endif

"------------------------------------------------------------------------------
" Don't use Ex mode, use Q for formatting
map Q gq

"------------------------------------------------------------------------------
" Clears search highlighting by just hitting a return
" The <BS> clears the command line.
" The final <CR: restores the standard <CR> behavior
"------------------------------------------------------------------------------
noremap <CR> :nohlsearch<CR>/<BS><CR>

"------------------------------------------------------------------------------
" CUA
"------------------------------------------------------------------------------
" cut = ^x
vmap  d
" copy = ^c
vmap  y
" undo = z
nmap  u


if( has("win32"))
    map ,v :e ~/_vimrc
else
    map ,v :e ~/.vimrc
endif

" toggle-number
"if number
"    set nonumber
"else
"    set number
"endif

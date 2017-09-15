"------------------------------------------------------------------------------
"
" exrc - configuration file for ex, vi, vim, elvis...
"
"------------------------------------------------------------------------------
" Copy indent from previous line
set autoindent

" Write files automagically before :make or :tag...
set noautowrite

" Do not toggle the 'g' and 'c' options of a substitution
set noedcompatible

" Please do not beep at me
set noerrorbells

" Turn lisp mode off by default
set nolisp

" Turn off list mode by default
set nolist

" Allow unescaped * and . in search patterns
set magic

" Allow for mappings to work recursivly
set remap

" Threshold for reporting number of lines changed
set report=2

" Number of lines to scroll with ^U (up) or ^D (down)
set scroll=0

" Specifies the nroff macros that separate sections
set sections=SHNHH\ HUnhsh

" Name of the shell to use for ! and :! commands
set shell=$SHELL

" Number of spaces to use for each step of (auto)indent
set shiftwidth=4

" Breifly jump to matching bracket
set showmatch

" Tell me what mode I am in
set showmode

" Number of spaces that a tab counts for
set tabstop=4

" Number of significant characters in a tag (zero is off)
set taglength=0

" Filenames for the tag command
set tags=./tags,./TAGS,tags,TAGS

" Name of the terminal
"set term=$TERM

" Wait for mapped keys?
set timeout

" Max line to scroll instead of redraw
"set ttyscroll=80

" Give a warning message when
" a shell command is used while the buffer has been changed
set warn

" Number of characters from the right window border where wrapping starts
set wrapmargin=2

" Searches wrap  around the file
set wrapscan

" Do not allow writing to any file without !
set nowriteany

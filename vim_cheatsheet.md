<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Vim Cheatsheet</title>
</head>
<body>

# Help #
Cmd            | Description                            |
------         | -----------                            |
F1             | Enter the help system                  |
\:help         | Enter the help system                  |
\:help keyword | Open help for keyword                  |
CTRL-]         | Jump to subject under the cursor       |
CTRL-O         | Jump back (repeat to go further back)  |
K              | Open man page on word under the cursor |

# Basics #
Cmd             | Description                                                     |
---             | -----------                                                     |
i               | Enter insert mode before the current character                  |
I               | Move to the beginning of the line and enter insert mode         |
ESC             | Return to normal mode                                           |
.               | Repeat the last change command                                  |
dd              | Delete a whole line                                             |
x               | Delete the character under the cursor                           |
J               | Join lines with one space in between                            |
gJ              | Join lines with no space in between                             |
u               | Undo                                                            |
U               | Undo all changes made on the last line that was edited          |
CTRL-r          | Redo or undo an undo                                            |
a               | Enter insert mode after the current character (append)          |
ea              | Enter insert mode after the current word (end, append)          |
A               | Enter insert mode after the last character of the line (Append) |
o               | Create a new empty line below the cursor and enter insert mode  |
O               | Create a new empty line above the cursor and enter insert mode  |
R               | Enter Replace mode                                              |
ZZ              | Write the file and exit                                         |
\:o[pen] {file} | Open file                                                       |
\:w[rite]       | Write the file                                                  |
\:saveas {file} | Save file as                                                    |
\:q[uit]        | Quit (exit)                                                     |
\:q[uit]!       | Quit force quit                                                 |

# Motion #
## Moving around ##
Cmd | Description                                                    |
--- | -----------                                                    |
h   | Move left                                                      |
j   | Move down                                                      |
gj  | Move down display line                                         |
k   | Move up                                                        |
gk  | Move up display line                                           |
l   | Move right                                                     |
^   | Move to the first non-blank character of the line              |
g^  | Move to the first non-blank character of the display line      |
0   | Move to the very first character of the line                   |
|   | Move to the very first character of the line                   |
g0  | Move to the very first character of the display line           |
_   | Move \<count\> - 1 lines down on the first non-blank character |
g\_ | Move to the last non-blank character of the line               |
$   | Move to the end of the line                                    |
g$  | Move to the end of the display line                            |
(   | Move to the previous sentence                                  |
)   | Move to the next sentence                                      |
{   | Move to the previous paragraph                                 |
}   | Move to the next paragraph                                     |

## Word movement ##
Cmd | Description                                   |
--- | -----------                                   |
w   | Move to the start of the next word            |
b   | Move backward, the start of the previous word |
e   | Move to the end of the next word              |
ge  | Move to the end of the previous word          |
W   | Move to the start of the next WORD            |
B   | Move backward, the start of the previous WORD |
E   | Move to the end of the next WORD              |
gE  | Move to the end of the previous WORD          |

## Moving to a character ##
Cmd    | Description                                                       |
---    | -----------                                                       |
f\<x\> | Single character search; move to the next instance of \<x\>       |
F\<x\> | Single character search; move to the previous instance of \<x\>   |
t\<x\> | Single character search; move to one char before the next \<x\>   |
T\<x\> | Single character search; move to one char before the prev \<x\>   |
;      | Repeat the last f,F,t or T search                                 |
,      | Repeat the last f,F,t or T search but reverse the direction       |
%      | Move to the matching pair (paren, brace or bracket) :h matchpairs |

## Moving to a specific line ##
Cmd    | Description                                      |
---    | -----------                                      |
gg     | Move to the first line of a file                 |
\<n\>G | Goto line n                                      |
\<n\>% | Move to a line that is n percent of the file     |
H      | Move to the first currently visible line (High)  |
M      | Move to the Middle of the visible lines (Middle) |
L      | Move to the Last visible line            (Low)   |

## Telling where you are ##
Cmd     | Description                                |
---     | -----------                                |
CTRL-g          | Show current file name and status  |
'ruler' option  | Show current line and column       |
'number' option | Show line numbers                  |

# Scrolling Around #
Cmd     | Description                                                      |
---     | -----------                                                      |
CTRL-e  | Move the viewport (scroll) down one line                         |
CTRL-y  | Move the viewport (scroll) up one line                           |
CTRL-f  | Move the viewport (scroll) forward (down) one screen             |
CTRL-b  | Move the viewport (scroll) back (up) one screen                  |
CTRL-u  | Move the viewport (scroll) up half a screen                      |
CTRL-d  | Move the viewport (scroll) down half a screen                    |
CTRL-e  | Scroll up one line                                               |
CTRL-y  | Scroll down one line                                             |
zz      | Scroll such that the current line is in the middle of the window |
zt      | Scroll such that the current line is at the top of the window    |
zb      | Scroll such that the current line is at the bottom of the window |

## Regex ##
Regex              | Description                               |
---                | -----------                               |
[bc]at             | Character class matches bat and/or cat    |
[a-z]              | Character class using character range     |
[\^:]\*            | Character class matches any char except : |
(exp1):(exp2)      | Group                                     |
{item}\*           | Matches 0 or more item                    |
{item}\\+          | Matches 1 or more item                    |
{item}\\=          | Matches 0 or 1 item                       |
{item}\\{3}        | Matches exactly 3 item                    |
{item}\\{3,5}      | Matches 3 to 5 item                       |
foo \| bar         | Matches foo or bar                        |
foo\\zs{match}\\ze | Create boundaries of the match            |

### Character classes ###
Regex  | Description                                        |
---    | -----------                                        |
\\a    | alphabetic                                         |
\\A    | non-alphabetic                                     |
\\d    | digit                                              |
\\D    | non-digit                                          |
\\w    | word char (alphanumeric and \_)                    |
\\W    | non-word char                                      |
\\x    | hex digit                                          |
\\X    | non-hex digit                                      |
\\s    | white space                                        |
\\S    | non-white space                                    |
\\l    | lowercase alpha                                    |
\\L    | non-lowercase alpha                                |
\\u    | uppercase alpha                                    |
\\U    | non-uppercase alpha                                |

# Search #
Cmd           | Description                                                  |
---           | -----------                                                  |
\/{regex}     | Search for a string                                          |
\/\\v{regex}  | Very magic, (,),\|,{ have special meaning                     |
\/\<x\>       | Search for the word "x"                                      |
\/\\v\<x\>      | Search for the word "x"                                      |
?{regex}      | Search backwards for a string                                |
n             | Find the Next occurance of the same string                   |
N             | Find the Next occurance in the opposite direction            |
gn            | Like n, but select match in visual mode or operate on match  |
gN            | Like N, but select match in visual mode or operate on match  | 
\*            | Search for the word under the cursor                         |
\#            | Search backwards for the word under the cursor               |
CTRL-r CTRL-w | Autocomplete the search field from the current preview match |
flag \\C      | Force case sensitivity                                       |

<p>Very magic is similar to Perl syntax.</p>
<p>to get a count of the search matches:<br/>
  * :%s/{regex}//gn<br/>
  * :vimgrep /regex/g {files}  - matches populate the quickfix list
</p>


### Search Offset ###
Cmd                | Description                                              |
---                | -----------                                              |
\/{regex}\/{num}   | num lines downwards in column 1                          |
\/{regex}\/+{num}  | num lines downwards in column 1                          |
\/{regex}\/-{num}  | num lines upwards in column 1                            |
\/{regex}\/e{+num} | num characters to the right of the end of the match      |
\/{regex}\/e{-num} | num characters to the left of the end of the match       |
\/{regex}\/s{+num} | num characters to the right of the start of the match    |
\/{regex}\/s{-num} | num characters to the left of the start of the match     |
\/{regex}\/b{+num} | num characters to the right of the start of the match    |
\/{regex}\/b{-num} | num characters to the left of the start of the match     |

## Search in multiple files ##
Cmd                          | Description                                   |
---                          | -----------                                   |
\:vimgrep /{regex}/ fileglob | Search for regex in multiple files            |

<p>The results of vimgrep populate the quickfix list</p>

# Marks and jumps #
Cmd     | Description                                                   |
---     | -----------                                                   |
\`\`    | Return to the position before the last jump                   |
\`.     | Return to the location of the last change                     |
\`^     | Return to the location of the last insert                     |
\`[     | Return to the start of the last change or yank                |
\`]     | Return to the end of the last change or yank                  |
\`<     | Return to the start of the last visual selection              |
\`>     | Return to the end of the last visual selection                |
CTRL-O  | Jump to previous (older) location in the jump list            |
CTRL-I  | Jump to next (newer) location in the jump list                |
TAB     | Jump to next (newer) location in the jump list                |
\:jumps | Show list of jump positions                                   |

m[a-z]  | Create named mark a through z                                 |
m[A-Z]  | Create global mark A through Z                                |
\`{a-z} | Jump to named mark a through z                                |
'{a-z}  | Jump to the beginning of the line containing mark a through z |
y\`{a-z}| Yank text to position of mark a through z                     |
\:marks | Show list of marks                                            |

# Visual Mode #
Cmd    | Description                                                    |
---    | -----------                                                    |
v      | Enter visual mode                                              |
V      | Enter visual line mode                                         |
CTRL-v | Enter visual block mode                                        |
o      | Moves cursor to other side of selected region                  |
O      | Moves cursor to other side of selected region different corner |
CTRL-g | Toggle Select Mode and Visual Mode                             |
gv     | Reselect the last visual selection                             |

<p>In visial block mode, I, A, c, C, J< magically operate on all lines in the
block./p>

# Moving text #
Cmd   | Description                                           |
---   | -----------                                           |
p     | Put the last deleted or yanked text after the cursor  |
P     | Put the last deleted or yanked text before the cursor |

## Copying text ##
Cmd                  | Description                                          |
---                  | -----------                                          |
y{motion}            | Copy target into the default register                |
y$                   | Copy to the end of the line                          |
Y / yy               | Copy the whole line                                  |
"{register}y{motion} | Yank target to a register                            |
y\`{a-z}             | Yank text to position of mark a through z            |

<p>In some cases (X Win, OS X) the 'current selection' maybe pasted in another
app without any other action.</p>

## Using the clipboard ##
Cmd   | Description                                             |
---   | -----------                                             |
"\*y   | Copy text to the 'current selection'                   |
"\*y$  | Copy to the end of the line to the 'current selection' |
"\*Y   | Copy the whole line to the 'current selection'         |
"\*yy  | Copy the whole line to the 'current selection'         |
"\*p   | Put from 'current selection'                           |
"+y   | Copy text to the clipboard                              |
"+y$  | Copy to the end of the line to the clipboard            |
"+Y   | Copy the whole line to the clipboard                    |
"+yy  | Copy the whole line to the clipboard                    |
"+p   | Put from clipboard                                      |

<p>The Edit-->[Cut, Copy, Paste] (Cmd-x, Cmd-c, Cmd-v in MacVim) menu items use the system clipboard to allow interaction with other applications.</p> 

## Using named registers ##
Cmd                           | Description                                   |
---                           | -----------                                   |
\:reg                         | Show registers content                        |
\:reg {register}              | Show contents of register  i                  |
"{register}y                  | Yank to a register                            |
"{register}y$                 | Yank to the end of the line to a register     |
"{register}Y                  | Yank the whole line to a register             |
"{register}yy                 | Yank the whole line to a register             |
"{register}d                  | Cut to a register                             |
"{register}p                  | Put after cursor from register                |
"{register}P                  | Put before cursor from register               |
insert mode: CTRL-r{register} | Paste the contents of a register              |
""                            | The unnamed (default) register                |
"0                            | The yank register                             |
"\_                           | The black hole register                       |
"+                            | The system clipboard register                 |
"\*                           | The X11 primary, use with middle mouse button |
"%                            | Name of the current file                      |
"#                            | Name of the alternate file                    |
".                            | Last inserted text                            |
":                            | Last Ex command                               |
"/                            | Last search pattern                           |

<p>Normal registers are lowercase (a-z). Uppercase register names (A-Z) are
used to append to the lowercase registers.</p>

# Text objects #
Cmd   | Description                                                 |
---   | -----------                                                 |
aw    | Around a word, including leading or trailing whitespace     |
aW    | Around a WORD, includeing leading or trailing whitespace    |
iw    | Inside word (noleading or trailing whitespace)              |
iW    | Inside WORD (noleading ot trailing whitespace              |
as    | Around a sentence including leading or trailing whitespace  |
is    | Inside a sentence including leading or trailing whitespace  |
ap    | Around a paragraph plus leading and trailing blank lines    |
ip    | Inside a Paragraph                                          |
a] a[ | Around a pair of [brackets]                                 |
i] i[ | Inside a pair of [brackets]                                 |
a) a( | Around a pair of (parentheses)                              |
ab    | Around a pair of (parentheses)                              |
i) i( | Inside a pair of (parentheses)                              |
ib    | Inside a pair of (parentheses)                              |
a> a< | Around a pair of <angle brackets>                           |
i> i< | Inside a pair of <angle brackets>                           |
at    | Around a pair of <xml>tags</xml>                            |
it    | Inside a pair of <anything>tags</anything>                  |
a} a{ | Around a pair of {braces}                                   |
aB    | Around a pair of {braces}                                   |
i} i{ | Inside a pair of {braces}                                   |
iB    | Inside a pair of {braces}                                   |
a"    | Around a pair of "double quotes"                            |
a'    | Around a pair of 'single quotes'                            |
a`    | Around a pair of `backticks`                                |
i"    | Inside a pair of "double quotes"                            |
i'    | Inside a pair of 'single quotes'                            |
i`    | Inside a pair of `backticks`                                |

# Keyword Autocompletion #
Cmd             | Description                                       |
---             | -----------                                       |
CTRL-p          | Keyword completion using multiple algorithims     |
CTRL-n          | Keyword completion using multiple algorithims     |
CTRL-x CTRL-n   | Keyword completion in the current file (forward)  |
CTRL-x CTRL-p   | Keyword completion in the current file (backward) |
CTRL-x CTRL-i   | Keyword completion in current and included files  |
CTRL-x CTRL-]   | Keyword completion from tags                      |
CTRL-x CTRL-o   | Keyword completion using function omnifunc        |
CTRL-x CTRL-u   | Keyword completion using user defined function    |
CTRL-x CTRL-k   | Keyword completion in 'dictionary'                |
CTRL-x CTRL-t   | Keyword completion in 'thesaurus'                 |
CTRL-x CTRL-l   | Complete an entire line                           |
CTRL-x CTRL-f   | File name complete                                |
CTRL-x CTRL-d   | Definition or macro completion                    |
CTRL-x CTRL-v   | Compete vim commands                              |
CTRL-y          | Accept enty in completion popup menu              |

<p>The option complete controls where generic keyword autocompletion gets its
source words. By default generic keyword autocomplete by scanning the loaded buffers, files included by loaded buffers and tag files.  If the option complete contains k, scan the files from the dictionary option.</p>


# Indenting #
Cmd             | Description                                       |
---             | -----------                                       |
\>\>            | Indent the current line                           |
\>{motion}      | Indent the target                                 |
\> (visual mode | Indent the selected lines                         |
\<\<            | Outdent  the current line                         |
\<{motion}      | Outdent the target                                |
\< (visual mode | Outdent the selected lines                        |

# Editing text #
## Delete #
### Normal Mode ###
Cmd       | Description                                     |
---       | -----------                                     |
x         | Delete char                                     |
xp        | Transpose two characters (delete and paste)     |
X         | Delete character left of the cursor (backspace) |
d{motion} | Delete target                                   |
D or d$   | Delete to the end of the line                   |
dd        | Delete a whole line                             |

### Visual Mode ###
Cmd       | Description                                     |
---       | -----------                                     |
d         | Delete the selected text                        |

## Change ##
### Normal Mode ###
Cmd       | Description                                                     |
---       | -----------                                                     |
r         | Replace a single character and return to normal mode            |
s         | Change one character and remain in insert mode                  |
c{motion} | Change (delete and enter insert mode)                           |
C or c$   | Change to the end of the line                                   |
S or cc   | Change a whole line                                             |

### Visual Mode ###
Cmd       | Description                                                     |
---       | -----------                                                     |
c or s    | Change the selected text                                        |

## Changing case ##
###  Normal Mode ###
Cmd        | Description                                    |
---        | -----------                                    |
~          | Change case of the character under the cursor  |
g~{motion} | Toggle case of target                          |
gu{motion} | Make target lower case                         |
gU{motion} | Make target upper case                         |
g~~        | Toggle case of line                            |
guu        | Make line lower case                           |
gUU        | Make line upper case                           |

### Visual Mode ###
Cmd        | Description                    |
---        | -----------                    |
~          | Toggle case of selection       |
U          | Make selected text upper case  |
u          | Make selected text lower case  |

## Formating ##
### Fill lines ###
Cmd   | Description                                                          |
---   | -----------                                                          |
gq    | Format (fill) textwidth option sets width of line, uses eternal prg  |
Q     | Alias for gq  if formatprg option is set (set formatprg=par\ -w72r)  |
qwip  | Format paragraph                                                     |

### Indenting code ###
Cmd        | Description                         |
---        | -----------                         |
==         | Indent line                         |
={motion}  | Indent target                       |
gg=G       | Indent the entire file              |
=          | In visual mode indent the selection |

# Macros #
Cmd                     | Description                              |
---                     | -----------                              |
q{register}             | Start recording macro into register. a-z |
q                       | End recording macro                      | 
@{register}             | Execute the macro stored in register.    |
{count}@{register}      | Execute the macro count times.           |
@@                      | Execute the last executed macro again    |

<p>A macro stored in a register can be edited by putting the macro to a buffer
("{register}p), editing the text and yanking the macro back into the register
("{register}y).</p>
<p>Parallel execution: Select lines in visual line mode.
Then :normal @{register}</p>

# Ex Mode #
## Ex Addressing ##
Cmd     | Description                                                     |
---     | -----------                                                     |
0       | Virtual line above the first line of the file                   |
1       | First line of the file                                          |
$       | Last line of the file                                           |
.       | Line where the cursor is                                        |
'm      | Line containing mark m                                          |
'<      | Start of visual selection                                       |
'>      | End of visual selection                                         |
%       | The entire file                                                 |
1,5     | Range lines 1 through 5                                         |
.+3,$-5 | Range line containing cursor + 3 to 5 lines above the last line |
 
## Ex Commands ##
Cmd                     | Description                                         |
---                     | -----------                                         |
\:[range]copy {address} | Copy lines                                          |
\:[range]t {address}    | Alias for copy (To)                                 |
\:[range]move {address} | Move lines                                          |
\:[range]delete         | Delete lines                                        |
\:[range]yank           | Yank lines                                          |
\:[range]normal {cmd}   | Execute a normal mode command on a range            |
\:[range]normal .       | Repeat last command over range                      |
\:[range]normal @q      | Repeat last macro over range                        |
\:@\:                   | Repeat last ex command                              |
\:[range]!{command}     | Filter range thru command                           |

    \:edit {file}           | Edit a new file                                     |
CTRL-^                  | Edit the alternate file                             |
\:oldfiles              | List recently edited files; \:e #<{recent_file_num} |
\:browse oldfiles       | List recently edited files and enter number of file |
\:write >> {file}       | Append to file                                      |
\:saveas {file}         | Save to a different file                            |

\:mksession {name}.vim  | Save the current session to a file                  |
\:source {name}.vim     | Restore a session from within vim                   |
vim -S {name}.vim       | Restore a session at startup                        |
\:wviminfo {name}i.viminfo| Save current viminfo to a file                    |
\:rviminfo {name}.viminfo | Restore viminfo from a file                       |

### Window Command ###
Cmd                     | Description                                         |
---                     | -----------                                         |
CTRL-w s                | Split the current window horizontally               |
\:split                 | Split the current window horizontally               |
\:split {file}          | Open a new window and edit file          |
\:new                   | Open a new window and edit new buffer    |

CTRL-w v                | Vertical split                           |
\:vsplit                | Vertical split                           |
\:vsplit {file}         | Vertical split and edit file             |
\:vnew                  | Vertical split and edit new buffer       |

CTRL-w q                | Quit a window                            |

\:close                 | Close a window                           |
CTRL-w o                | Close all other windows (only)           |
CTRL-w CTRL-o           | Close all other windows (only)           |
\:only                  | Close all other windows                  |

CTRL-w w                | Jump to other window                     |
CTRL-w CTRL-w           | Jump to other window                     |
CTRL-w h                | Move to the window on the left           |
CTRL-w <left>           | Move to the window on the left           |
CTRL-w j                | Move to the window below                 |
CTRL-w <down>           | Move to the window below                 |
CTRL-w k                | Move to the window above                 |
CTRL-w <up>             | Move to the window above                 |
CTRL-w l                | Move to the window on the right          |
CTRL-w <right>          | Move to the window on the right          |
CTRL-w t                | Move to the TOP window                   |
CTRL-w b                | Move to the BOTTOM window                |

CTRL-w K                | Move the current window to the top       |
CTRL-w J                | Move the current window to the bottom    |
CTRL-w H                | Move the current window to the far left  |
CTRL-w L                | Move the current window to the far right |

CTRL-w +                | Increase size of window                  |
CTRL-w -                | Decrease size of window                  |
z{height}<CR>           | Redraw making window {height} lines tall |
CTRL-w =                | Equalize height and width of all windows |
CTRL-w _                | Maximize height of the current win       |
CTRL-w |                | Maximize width of the current win        |

\:qall                  | Close all windows and exit vim           |
\:qall!                 | Force close all windows and exit vim     |
\:wall                  | Write all windows                        |
\:wqall                 | Write and close all windows; exit vim    |

\:pwd                   | Print working directory                  |
\:cd {dir}              | Change directory to {dir}                |
\:cd -                  | Change to the previous directory         |
\:lcd {dir}             | Change directory for the current window  |

\:argdo {command}       | Execute command on each argument         |
\:windo {command}       | Execute command on each window           |
\:bufdo {command}       | Execute command on each buffer           |

### Tags and definitions ###
Cmd                     | Description                                         |
---                     | -----------                                         |
\:set tags={path}       | Use specific tags file                              |
\:set tags={n1},{n2}... | Tag file names to look for                          |
CTRL-]                  | Jump to definition of tagname under cursor          |
\:tag {tagname}         | Jump to definition of tagname                       |
\:tag                   | Advance through tag history                         |
\:pop                   | Reverse through tag history                         |
CTRL-t                  | Reverse through tag history                         |
\:tags                  | Show list of tags you have traversed                |
\:stag {tagname}        | Split the current window and jump to def of tagname |
CTRL-W ]                | Split the current window and jump to tag under curs |
gCTRL-]                 | If multiple matches, provide a list to choose from  |
\:tjump {tagname}       | If multiple matches, provide a list to choose from  |
\:tselect {tagname}     | Select which match to jump to                       |
[t                      | Goto the previous tag in the taglist (:tprevious)   |
]t                      | Goto the next tag in the taglist (:tnext)           |
[T                      | Goto the first tag in the taglist (:tfirst)         |
]T                      | Goto the last tag in the taglist (:tlast)           |
\:ptag {tagname}        | Open a preview window to display tag                |
\:pclose                | Close the preview window                            |
gD                      | Goto definition in current file                     |
gd                      | Goto definition in current block                    |

# Substitution or search and replace ##
Cmd                          | Description                                |
---                          | -----------                                |
\:[range]s/from/to/[flags]   | Change "from" string to "to" string        |
\:%s/from/to/                | % = Whole file                             |
\:%s/from/to/                | No flags = first occurrence each line      |
\:%s/from/to/g               | g (global) every occurence                 |
\:%s/from/to/p               | p print the last changed line              |
\:%s/from/to/c               | c (confirm) ask for confirmation           |
\:%s/from/to/e               | e not finding a match is not an error      |
\:%s/from//n                 | Report the number of matches               |
\:%s/from_regex/to/          | The from can be a regex                    |
\:s/\\%V{from}/to/g          | Replace within a visual selection          |

### Replacement string special characters ###
Symbol          | Description                                                |
------          | -----------                                                |
\\r             | Carrage return                                             |
\\t             | Tab                                                        |
\\\\            | Backslash                                                  |
\\1             | First submatch                                             |
\\0             | The entire matched pattern                                 |
~               | Use {string} from the previous invocation of \\:substitute |
\\={vim script} | Evaluate vim script use result as string. \\=@a register a |

## global command ##
Cmd                         | Description                                |
---                         | -----------                                |
\:[range]g /{regex}/ {cmd}  | Match with regex and execute command there |
\:g[lobal]! /{regex}/ {cmd} | Execute command on lines that do not match |
\:g/{regex}/d               | Delete matching lines                      |

<p>The default range is the entire file (%). g[lobal]! </p>

# Files and Buffers #
Cmd                          | Description                                  |
---                          | -----------                                  |
gf                           | Find and edit the file name under the cursor |
\:find {filename}            | Find and edit {filename} uses path option    |
\:ls                         | List buffers                                 |
\:buffer {buffer}            | Edit buffer                                  |
\:bnext or \:bn              | Go to the next buffer                        |
\:bprev or \:bp              | Go to the previous buffer                    |
\:bdelete {buffer}           | Remove a buffer from the list                |
\:bwipe {buffer}             | Really make vim forget about a buffer        |
<p>The path option is a list of place to look for files using gf or find.  The
suffixesadd option is a list of file extensions Vim will attempt when looking
up a filename.</p>

# Vim diff #
Cmd                          | Description                              |
---                          | -----------                              |
\:vertical diffsplit {file}  | Diff 'file' with current file            |
]c                           | Jump to next change                      |
[c                           | Jump to previous change                  |
\:diffupdate                 | Update highlighting                      |
dp                           | Diff put; copy change to other window    |
do                           | Diff obtain; copy text from other window |

# Vim spellchecking #
Cmd          | Description                                              |
---          | -----------                                              |
\:set spell! | Toggle spellchecking; mapped to ,s                       |
]s           | Next spelling error                                      |
[s           | Previous spelling error                                  |
z=           | Display list 0f suggested corrections                    |
zg           | Add word to local spellfile ~/.vim/spell/en.utf-8.add    |
zw           | Remove word from local spellfile                         |
zug          | Undo zg                                                  |
zuw          | Undo zw                                                  |

# More external programs #
## More filtering ##
Cmd                 | Description                                       |
---                 | -----------                                       |
!{motion}{program}  | Filter block of text thru program                 |
!!{program}         | Filter the current line thru program              |
!!date              | Replaces the current line with the output of date |

## Reading/writing from/to a command ##
Cmd                 | Description                                       |
---                 | -----------                                       |
\:read !{program}   | Read output of program into the file              |
\:write !{program}  | Write file to input of program                    |

# Tabs #
Cmd                    | Description                                       |
---                    | -----------                                       |
\:tabnew {filename}    | Open {filename in a new tab                       |
\:tabe[dit] {filename} | Open {filename in a new tab                       |
\:tabc[lose]           | Close the current tab page and all of its windows |
\:tabo[nly]            | Keep the active tab page, closing all others      |
Ctrl-w T               | Move the current window into its own tab          |
\:tabn[ext] or gt      | Switch to the next tab page                       |
[N] gt                 | Switch to tab number N                            |
\:tabp[revious] or gT  | Switch to the previous tab page                   |
\:tabm[ove] [N]        | Move tab                                          |
\:tabdo command        | Run command on all tabs                           |

# Quickfix #
Cmd                    | Description                                         |
---                    | -----------                                         |
\:copen                | Open the quickfix window                            |
\:cclose               | Close the guickfix window                           |
\:cdo {command}        | Execute {command} on each line in the quickfix list |
\:cfdo {command}       | Execute {command} on each file in the quickfix list |
\:cprevious            | Goto the previous item in the quickfix list         |
[q                     | Goto the previous item in the quickfix list         |
\:cnext                | Goto the next item in the quickfix list             |
]q                     | Goto the next item in the quickfix list             |
\:cfirst               | Goto the first item in the quickfix list            |
[Q                     | Goto the first item in the quickfix list            |
\:clast                | Goto the last item in the quickfix list             |
]Q                     | Goto the last item in the quickfix list             |
\:cnfile               | Goto the first item in next file                    |
\:cpfile               | Goto the lasr item in previous file                 |

<p>\:make \:grep and \:vimgrep use the quickfix list.</p>

# Location List #
Cmd              | Description                                                |
---              | -----------                                                |
\:lopen          | Open the location list window                              |
\:lclose         | Close the window showing the location list for the cur win |
\:ldo {cmd}      | Execute {cmd} on each line in location list for cur win    |
\:lfdo {cmd}     | Execute {cmd} on each file in location list for cur win    |
\:lprevious      | Goto the previous item in the location list                |
[l               | Goto the previous item in the location list                |
\:lnext          | Goto the next item in the location list                    |
]l               | Goto the next item in the location list                    |
\:lfirst         | Goto the first item in the location list                   |
[L               | Goto the first item in the location list                   |
\:llast          | Goto the last item in the location list                    |
]L               | Goto the last item in the location list                    |

<p>\:make \:lgrep and \:lvimgrep use the location list.</p>


# Folding #
Cmd         | Description                                         |
---         | -----------                                         |
zf{motion}  | Create fold set; foldcolumn=4 to see folds          |
zd          | Delete fold                                         |
zo          | Open fold                                           |
zc          | Close fold                                          |
zr          | Open all folds at a level                           |
zm          | Close all folds at a level                          |
zR          | Open all folds                                      |
zM          | Close all folds                                     |
zn          | Disable folding                                     |
zN          | Re-enable folding                                   |
zi          | Toggle folding                                      |

<p>The option foldmethod controls the kind of folding used for the current
window. set foldmethod=syntax</p>

# Programming comments (with vim-commentary) #
Cmd   | Description                             |
---   | -----------                             |
gc{motion}| Toggle comments on target           |
gcc       | Toggle comment on the current line  |

# vim-surround #
Cmd   | Description                                                          |
---   | -----------                                                          |
yss{sobj}          | Surround the current line with {sobj}                   |
ys{motion}{sobj}   | Surround target with {sobj} ({[ add space )}] do not    |
ys{motion}a        | Surround target with < >                                |
ys{motion}b        | Surround target with ( )                                |
cs{sobj}{sobj}     | Change {sobj}                                           |
cst{sobj}          | Change a "surround" that is a tag to {sobj}             |
ds{sobj}           | Remove "surround"                                       |
visual mode S{sobj}|  Surround target with {sobj}                            |

Original Text            | Cmd      | Result                           |
-------------            |          |                                  |
"He[l]lo, world!"        | ds"      | [H]ello, world!                  |
\<p\>Start[ ]here\</p\>  | dst      | [S]tart here                     | 
callX(var1, var2)        | ds(      | callXvar1, var2                  |
"Sing[l]e quoted"        | cs"'     | 'Sing[l]e quoted'                |
'Look to the east'       | cs'\<p\> | [\<]p\>Look to the east\</p\>    |
func [a] literal string  | ys$"     | func ["]a literal string"        |
if [v]ar1 && var2        | ys3w)    | [(]var1 && var2)                 |
34,[ ]21, 7              | yss{     | [{] 34, 21, 7 }                  |
Jack said, ["]Done it!"  | ysf"t    | Jack said, [<]em>"Done it!"</em> |
Add [t]wo eggs           | viwS"    | Add ["]two" eggs                 |


# vim-unimpaired #
Cmd   | Description                                                          |
---   | -----------                                                          |
[a    | Edit the previous file in the argument list (:previous)              |
]a    | Edit the next file in the argument list (:next)                      |
[A    | Edit the first file in the argument list (:first)                    |
]A    | Edit the last file in the argument list (:last)                      |
[b    | Edit the previous file in the buffer list (:bprevious)               |
]b    | Edit the next file in the buffer list (:bnext)                       |
[B    | Edit the first file in the buffer list (:first)                      |
]B    | Edit the last file in the buffer list (:last)                        |
[l    | Goto the previous location in the location list (:lprevious)         |
]l    | Goto the next location in the location list (:lnext)                 |
[L    | Goto the first location in the location list (:first)                |
]L    | Goto the last location in the location list (:last)                  |
[q    | Goto the previous item in the quickfix list (:cprevious)             |
]q    | Goto the next item in the quickfix list (:cnext)                     |
[Q    | Goto the first item in the quickfix list (:cfirst)                   |
]Q    | Goto the last item in the guickfix list (:clast)                     |
[t    | Goto the previous tag in the taglist (:tprevious)                    |
]t    | Goto the next tag in the taglist (:tnext)                            |
[T    | Goto the first tag in the taglist (:tfirst)                          |
]T    | Goto the last tag in the taglist (:tlast)                            |

# Tabular - Aligning text #
<p>:Tab(ularize) /-  will align on =</p>

# HTML (with emmet) #
Cmd      | Description                                                       |
---      | -----------                                                       |
CTRL-A,  | Expand abbrev emmet style                                         |

# Tabs #
<p>To convert tabs to spaces:<br/>
:set expandtab<br/>
:retab!<br/>
</p>
<p>To convert spaces to tabs:<br/>
:set noexpandtab<br/>
:retab!<br/>
</p?
</body>
</html>


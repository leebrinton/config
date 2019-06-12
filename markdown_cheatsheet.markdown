Title: Markdown Cheatsheet

Markdown Cheatsheet
===================

<div name="inline_html">
    <p>You can in-line html blocks at any time.</p>
</div>

Block Elements
--------------

### Headers

# H1 #

## H2 ##

### H3 ###

#### H4

##### H5

###### H6


### Blockquotes

> Blockquote text
> > Nested blockquote
> txet etouqkcolb

### Lists

* unordered list item1
* unordered list item2
* unordered list item3

- unordered list item1
- unordered list item2
- unordered list item3

+ unordered list item1
+ unordered list item2
+ unordered list item3

1. ordered list item1
2. ordered list item2
3. ordered list item3

### Code Blocks

```c
for (i = 0; i < num; i++) {
    process(i);
}
```

Text indented 4 spaces of 1 tab is a codeblock.

    int i;
    for( i = 0; i < 10; i++ )
        printf( "%d\n", i );


### Horizontal Rules

* * *

- - -

___

Span Elements
--------------

### Links

I love my [Mac](http://www.apple.com/macbook-pro/ "MacBook Pro")!

or

You can embed a ref and define it later. [Mac][macbookpro]

[macbookpro]: http://www.apple.com/macbook-pro/ "MacBook Pro"

### Emphasis

*emphasis*
_emphasis_

**strong**
__strong__
    
### Code

Use the `printf()` function.

### Images

![Powered By OSX](/Library/WebServer/Documents/PoweredByMacOSX.gif)
![Powered By OSX](/Library/WebServer/Documents/PoweredByMacOSX.gif "OSX")

or

You can embed a ref and define it later. ![Powered By OSX][osximage]

[osximage]: /Library/WebServer/Documents/PoweredByMacOSX.gif "OSX" width=100px
[^MultiMarkdown]
 
Miscellaneous
--------------

### Automatic links

The Sesco website is <http://www.sescollc.com>.

An email address is <lbrinton@sescollc.com>

### Backslash Escapes

Markdown allows you to use backslash escapes to generate literal characters which would otherwise have special meaning in Markdown's formatting syntax.

For example, if you wanted to surround a word with literal asterisks (instead of an HTML `<em>` tag), you can use backslashes before the asterisks, like this:

\*literal asterisks\*

Markdown provides backslash escapes for the following characters:


Char  | Char Name           |
 :--- | :---------          |
\     | backslash           |
`     | backtick            |
*     | asterisk            |
_     | underscore          |
{}    | curly braces        |
[]    | square brackets     |
()    | parentheses         |
#     | hash mark           |
+     | plus sign           |
-     | minus sign (hyphen) |
.     | dot                 |
!     | exclamation mark    |
[Backslash escapable character table]

[^MultiMarkdown]

[^MultiMarkdown]: This uses a MultiMarkdown extention to the Markdown language

GFM (Github Flavered Markdown)
------------------------------

### Task lists

- [x] This is a completed item
- [ ] This is a incomplete item

### SHA references

<p>Any reference to a commit’s SHA-1 hash will be automatically converted
  into a link to that commit on GitHub:</p>

```
4ad0c921206dec4d1518f4aeead932e7617f934f
denysdovhan/how-to-markdowkn@4ad0c921206dec4d1518f4aeead932e7617f934f
```

### Issue and Pull request references

<p>Any number that refers to an Issue or Pull Request will be automatically
converted into a link:</p>

```
#1
denysdovhan/how-to-markdowkn#1
```

 ### Username @mentions

<p>Typing an @ symbol, followed by a username, will notify that person to
  come and view the comment. This is called a @mention, because you’re
  mentioning the individual. You can also mention @teams within an
  organization.</p>

 ### Emoji

It's a funny part, but it's still important. GFM also supports emoji!

```
  ✨ 🐫 💥
```

To see a list of every emoji that is supported, check out the [Emoji Cheat
Sheet](http://www.emoji-cheat-sheet.com/).


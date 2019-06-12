const activate = oni => {
    // access the  Oni plugin API here
    //
    // for example, unbind  the defualt <c-p> action:
    // oni.input.unbind("<c-p>")

    // or bind a new action:
    oni.input.bind("<c-enter>", () => alert("Pressed control enter"))
    oni.input.bind("<f12>", "markdown.togglePreview")
}

module.exports = {
    activate,
    // change configuration values here:
    //"achievements.enabled": true,

    //"autoClosingPairs.enabled": true,
    //"autoClosingPairs.default": [
    //    { open: "{", close: "}" },
    //    { open: "[", close: "]" },
    //    { open: "(", close: ")" },
    //],

    // Set whether or not Oni should check for updates on startup.
    "autoUpdate.enabled": true,

    // Set the default url that the browser window opens to when selecting
    // Browser: Vertical Split or Browser: Horizontal Split from the command
    // palette
    //"browser.defaultUrl": "https://duckduckgo.com",
    // Set whether or not the embedded browser is enabled.
    //"browser.enabled": true,    
    //"browser.zoomFactor": 1.0,
    
    //"configuration.editor": "typescript",
    //"configuration.showReferenceBuffer": true,

    // If the externalised commandline should be enabled or not. Setting this
    // to false will revert to the standard Vim commandline at the bottom of
    // the screen.
    //"commandline.mode": true,
    // The commandline employs Icons to replace / and ? for searching, which
    // default to on. Setting this to false will remove the search icons when
    // using / or ?.
    //"commandline.icons": true,
    "commandline.mode": false,
    "commandline.icons": false,

    //"debug.fixedSize": null,
    //"debug.neovimPath": null,
    //"debug.persistOnNeovimExit": false,
    //"debug.detailedSessionLogging": false,
    //"debug.showTypingPrediction": false,
    //"debug.showNotificationOnError": process.env.NODE_ENV !== "production",
    //"debug.fakeLag.languageServer": null,
    //"debug.fakeLag.neovimInput": null,

    // Specifies stretch factor for background image (initial, cover, contain)
    //"editor.backgroundImageSize": "cover",
    // Path to a custom background image
    //"editor.backgroundImageUrl": null,
    //Opacity of background image.
    //"editor.backgroundOpacity": 1.0,
    
    // Enables / disables system Clipboard Integration.
    // All yanks or deletes will be pushed to the system clipboard.
    // Pressing <C-c> on Windows/Linux (<M-c> on OSX) in visual mode will copy
    // the selected text to the system clipboard.
    // Pressing <C-v> on Windows/Linux (<M-v> on OSX) in insert mode will paste
    // the text from the system clipboard.
    // If you have custom behavior or functionality bound to <C-c>, <C-v> (or
    // <M-c>, <M-v> on OSX), you may wish to disable this behavior by setting
    // editor.clipboard.enabled to false.
    //"editor.clipboard.enabled": true,
    //"editor.clipboard.synchronizeDelete": false,
    //"editor.clipboard.synchronizeYank": true,

    // Default completion strategy, 'hidden' uses an externalized popupmenu, and
    // 'native' uses the native Vim popupmenu. Can be one of 'oni', 'hidden',
    // or 'native'. 'oni' uses Oni's default completion strategy, 'hidden' uses
    // an externalized popupmenu, and 'native' uses the native Vim popupmenu.
    //"editor.completions.mode": "oni",
    // Enables / disables cursor column highlight.
    //"editor.cursorColumn": false,
    // Defines opacity of cursor column highlight.
    // Only valid when editor.cursorColumn: true.
    //"editor.cursorColumnOpacity": 0.1,
    // Enables / disables cursor line highlight.
    //"editor.cursorLine": true,
    // Defines opacity of cursor line highlight. Only valid when
    // editor.cursorLine: true.
    //"editor.cursorLineOpacity": 0.1,

    //"editor.definition.enabled": true,

    // Enables / disables showing details when cursor is over an error.
    //"editor.errors.slideOnFocus": true,

    // Font family. There is a problem on MacOS when a font has multiple
    // typefaces. If you want to select a specific typeface instead of the
    // default one, try using the filename containing the specific typeface,
    // See this issue N.B.: For macOS users to find the PostScript font name
    // for a font, you can open up the Font Book app, select the font you want
    // to use, and type command-i.
    // (default: Windows Consolas, OSX Menlo, Linux DejaVu Sans Mono)
    //"editor.fontFamily": "",
    //"editor.fontFamily": "Monaco",
    // Font size (default: Windows 11px, OSX 10px, Linux 11px)
    //"editor.fontSize": "12px",
    //"editor.fontLigatures": true,

    // Perform language-specific "formatting" operation when returning
    // to Normal mode
    //"editor.formatting.formatOnSwitchToNormalMode": false,
    
    //"editor.fullScreenOnStart": false,

    //"editor.imageLayerExtensions": [".gif", ".jpg", ".jpeg", ".bmp", ".png"],

    // Padding between lines, in pixels.
    //"editor.linePadding": 2,
    //"editor.maximizeScreenOnStart": false,
    //"editor.maxLinesForLanguageServices": 2500,

    // Enables / disables QuickInfo popup when hovering over known method or
    // variable in a supported file type. The editor.quickInfo.show command can
    // be bound to a key in order to explicitly open it - Key Bindings
    //"editor.quickInfo.enabled": true,
    // Delay (in ms) for showing QuickInfo when the cursor is on a term.
    //"editor.quickInfo.delay": 500,

    //"editor.quickOpen.alternativeOpenMode": Oni.FileOpenMode.ExistingTab,
    //"editor.quickOpen.defaultOpenMode": Oni.FileOpenMode.Edit,
    // If specified, executes a custom command to populate the fuzzy finder.
    // For example, fzf or ls. Value should contain a placeholder ${search}
    // which will be replaced with user's input to fuzzy finder.
    //"editor.quickOpen.execCommand": null,
    //"editor.quickOpen.filterStrategy": "vscode",

    //"editor.renderer": "canvas",

    // Sets whether the buffer scrollbar is visible.
    //"editor.scrollBar.visible": true,
    // Sets whether the cursor tick within the buffer scrollbar is visible.
    //"editor.scrollBar.cursorTick.visible": true,
    
    //"editor.split.mode": "native",

    // Set to true to show scope information for a token. This is helpful when
    // figuring out what to set for the scope value for editor.tokenColors.
    //"editor.textMateHighlighting.debugScopes": false,
    // Oni features syntax highlighting based on TextMate Grammars. These
    // offer more 'smarts' about the language keywords, versus the out-of-box
    // Vim syntax regions.
    //"editor.textMateHighlighting.enabled": false,
    // An array of optional overrides, ex: [{ scope: "variable.object"},
    // settings: "Identifier" }]. Currently, settings must correspond to a
    // Vim highlight group, and scope must correspond to a TextMate scope.
    // This configuration is only used when
    // experimental.editor.textMateHighlighting.enabled is true.
    //"editor.tokenColors": [],
    
    //"editor.typingPrediction": true,

    // Sets additional paths for binaries. This may be necessary to configure,
    // if using plugins or a Language Server that is not in the default set of
    // runtime paths. Note that depending on where you launch Oni, there may be
    // a different set of runtime paths sent to it - you can always check by
    // opening the developer tools and running process.env.PATH in the console.
    "environment.additionalPaths": [
        "/opt/local/bin",
        "/usr/local/bin",
        "/usr/bin",
    ],

    //"explorer.maxUndoFileSizeInBytes": 500_000,
    //"explorer.persistDeletedFiles": true,

    //"keyDisplayer.showInInsertMode": false,

    //"language.c.languageServer.command": "clangd",
    
    //"language.clojure.textMateGrammar": path.join(
    //    __dirname,
    //    "extensions",
    //    "clojure",
    //    "syntaxes",
    //    "clojure.tmLanguage.json",
    //),

    //"language.cpp.languageServer.command": "clangd",

    //"language.css.languageServer.command": cssLanguageServerPath,
    //"language.css.languageServer.arguments": ["--stdio"],
    //"language.css.textMateGrammar": path.join(
    //    __dirname,
    //    "extensions",
    //    "css",
    //    "syntaxes",
    //    "css.tmLanguage.json",
    //),
    //"language.css.tokenRegex": "[$_a-zA-Z0-9-]",

    //"language.go.languageServer.command": "go-langserver",
    //"language.go.textMateGrammar": path.join(__dirname, "extensions", "go", "syntaxes", "go.json"),

    //"language.haskell.languageServer.command": "stack",
    //"language.haskell.languageServer.arguments": ["exec", "--", "hie", "--lsp"],
    //"language.haskell.languageServer.rootFiles": [".git"],
    //"language.haskell.languageServer.configuration": {},
    //"language.html.languageServer.command": htmlLanguageServerPath,
    //"language.html.languageServer.arguments": ["--stdio"],

    //"language.java.textMateGrammar": {
    //    ".java": path.join(__dirname, "extensions", "java", "syntaxes", "Java.tmLanguage.json"),
    //    ".jar": path.join(__dirname, "extensions", "java", "syntaxes", "Java.tmLanguage.json"),
    //},

    //"language.javascript.completionTriggerCharacters": [".", "/", "\\"],
    //"language.javascript.textMateGrammar": {
    //    ".js": path.join(
    //        __dirname,
    //        "extensions",
    //        "javascript",
    //        "syntaxes",
    //        "JavaScript.tmLanguage.json",
    //    ),
    //    ".jsx": path.join(
    //        __dirname,
    //        "extensions",
    //        "javascript",
    //        "syntaxes",
    //        "JavaScriptReact.tmLanguage.json",
    //    ),
    //},

    //"language.less.languageServer.command": cssLanguageServerPath,
    //"language.less.languageServer.arguments": ["--stdio"],
    //"language.less.textMateGrammar": path.join(
    //    __dirname,
    //    "extensions",
    //    "less",
    //    "syntaxes",
    //    "less.tmLanguage.json",
    //),
    //"language.less.tokenRegex": "[$_a-zA-Z0-9-]",

    //"language.lua.textMateGrammar": path.join(
    //    __dirname,
    //    "extensions",
    //    "lua",
    //    "syntaxes",
    //    "lua.tmLanguage.json",
    //),

    //"language.markdown.textMateGrammar": {
    //    ".md": path.join(
    //        __dirname,
    //        "extensions",
    //        "markdown",
    //        "syntaxes",
    //        "markdown.tmLanguage.json",
    //    ),
    //    ".markdown": path.join(
    //        __dirname,
    //        "extensions",
    //        "markdown",
    //        "syntaxes",
    //        "markdown.tmLanguage.json",
    //    ),
    //    ".mkd": path.join(
    //        __dirname,
    //        "extensions",
    //        "markdown",
    //        "syntaxes",
    //        "markdown.tmLanguage.json",
    //    ),
    //    ".mdown": path.join(
    //        __dirname,
    //        "extensions",
    //        "markdown",
    //        "syntaxes",
    //        "markdown.tmLanguage.json",
    //    ),
    //},

    //"language.objc.textMateGrammar": {
    //    ".m": path.join(
    //        __dirname,
    //        "extensions",
    //        "objective-c",
    //        "syntaxes",
    //        "objective-c.tmLanguage.json",
    //    ),
    //    ".h": path.join(
    //        __dirname,
    //        "extensions",
    //        "objective-c",
    //        "syntaxes",
    //        "objective-c.tmLanguage.json",
    //    ),
    //},

    //"language.objcpp.textMateGrammar": {
    //    ".mm": path.join(
    //        __dirname,
    //        "extensions",
    //        "objective-c++",
    //        "syntaxes",
    //        "objective-c++.tmLanguage.json",
    //    ),
    //},

    //"language.ocaml.languageServer.command": ocamlLanguageServerPath,
    //"language.ocaml.languageServer.arguments": ["--stdio"],
    //"language.ocaml.languageServer.configuration": ocamlAndReasonConfiguration,

    //"language.php.textMateGrammar": path.join(
    //    __dirname,
    //    "extensions",
    //    "php",
    //    "syntaxes",
    //    "php.tmLanguage.json",
    //),
    
    //"language.python.languageServer.command": "pyls",
    //"language.python.textMateGrammar": path.join(
    //    __dirname,
    //    "extensions",
    //    "python",
    //    "syntaxes",
    //    "python.tmLanguage.json",
    //),

    //"language.reason.languageServer.command": ocamlLanguageServerPath,
    //"language.reason.languageServer.arguments": ["--stdio"],
    //"language.reason.languageServer.rootFiles": [".merlin", "bsconfig.json"],
    //"language.reason.languageServer.configuration": ocamlAndReasonConfiguration,
    //"language.reason.textMateGrammar": path.join(
    //    __dirname,
    //    "extensions",
    //    "reason",
    //    "syntaxes",
    //    "reason.json",
    //),

    //"language.ruby.textMateGrammar": path.join(
    //    __dirname,
    //    "extensions",
    //    "ruby",
    //    "syntaxes",
    //    "ruby.tmLanguage.json",
    //),
    
    //"language.rust.textMateGrammar": path.join(
    //    __dirname,
    //    "extensions",
    //    "rust",
    //    "syntaxes",
    //    "rust.tmLanguage.json",
    //),

    //"language.scss.languageServer.command": cssLanguageServerPath,
    //"language.scss.languageServer.arguments": ["--stdio"],
    //"language.scss.textMateGrammar": path.join(
    //    __dirname,
    //    "extensions",
    //    "scss",
    //    "syntaxes",
    //    "scss.json",
    //),
    //"language.scss.tokenRegex": "[$_a-zA-Z0-9-]",

    //"language.sh.textMateGrammar": path.join(
    //    __dirname,
    //    "extensions",
    //    "shell",
    //    "syntaxes",
    //    "shell.tmLanguage.json",
    //),

    //"language.swift.textMateGrammar": path.join(
    //    __dirname,
    //    "extensions",
    //    "swift",
    //    "syntaxes",
    //    "swift.tmLanguage.json",
    //),

    //"language.typescript.completionTriggerCharacters": [".", "/", "\\"],
    //"language.typescript.textMateGrammar": {
    //    ".ts": path.join(
    //        __dirname,
    //        "extensions",
    //        "typescript",
    //        "syntaxes",
    //        "TypeScript.tmLanguage.json",
    //    ),
    //    ".tsx": path.join(
    //        __dirname,
    //        "extensions",
    //        "typescript",
    //        "syntaxes",
    //        "TypeScriptReact.tmLanguage.json",
    //    ),
    //},

    //"language.zsh.textMateGrammar": path.join(
    //    __dirname,
    //    "extensions",
    //    "shell",
    //    "syntaxes",
    //    "shell.tmLanguage.json",
    //),

    // Sets whether or not the learning feature is enabled (this includes the
    // learning sidebar and the interactive tutorials)
    //"learning.enabled": true,

    //"menu.caseSensitive": "smart",
    //"menu.rowHeight": 40,
    //"menu.maxItemsToShow": 8,

    //"notifications.enabled": true,

    //"oni.audio.bellUrl": null,
    //Array of files to show in fuzzy finder if Oni is launched via application
    //icon (no working directory).
    //"oni.bookmarks": [],
    // If true, hide menu bar. When hidden, menu bar can still be displayed
    // with Alt.
    //"oni.hideMenu": false,
    // Glob pattern of files to exclude from Fuzzy Finder (Ctrl-P).
    //oni.exclude (default: ["**/node_modules/**"])
    // This determines whether the user's init.vim is loaded. This setting
    // can be a string path or a boolean. If set to true, it will load init.vim
    // from the default location. If set to a string path (ie,
    // 'C:/somedir/myinit.vim'), this will load init.vim from the specified
    // path (useful if you want a separate config between Oni & Neovim). Use
    // caution when setting this and setting oni.useDefaultConfig to true, as
    // there could be conflicts with the default configuration.
    "oni.loadInitVim": true

    //"oni.plugins.prettier": {
    //    settings: {
    //        semi: false,
    //        tabWidth: 2,
    //        useTabs: false,
    //        singleQuote: false,
    //        trailingComma: "es5",
    //        bracketSpacing: true,
    //        jsxBracketSameLine: false,
    //        arrowParens: "avoid",
    //        printWidth: 80,
    //    },
    //    formatOnSave: false,
    //    enabled: false,
    //},
    // ONI comes with an opinionated default set of plugins for a predictable
    // out-of-box experience. This will be great for newcomers to ONI or Vim,
    // but for Vim/Neovim veterans, this will likely conflict. Set this to
    // false to avoid loading the default config, and to load settings from
    // init.vim instead (If this is false, it implies oni.loadInitVim is true)
    //"oni.useDefaultConfig": true,
    // Sets the popupmenu_external option in Neovim. This will override the
    // default UI to show a consistent popupmenu, whether using Oni's
    // completion mechanisms or VIM's.
    // Use caution when changing the menuopt parameters if using a custom
    // init.vim, as that may cause problematic behavior
    //"oni.useExternalPopupMenu": true,

    // Automatically copies the screen of Oni to the system clipboard.
    //"recorder.copyScreenshotToClipboard": false,
    // Sets where Oni will save any recordings or screenshots to.
    //"recorder.outputPath": os.tmpdir(),

    // Enable a sidebar on the left side, containing a File Explorer and other
    // useful features.
    //"sidebar.enabled": true,
    //"sidebar.default.open": true,
    //"sidebar.width": "15em",

    //"sidebar.marks.enabled": false,
    //"sidebar.plugins.enabled": false,

    //"snippets.enabled": true,
    //"snippets.userSnippetFolder": null,

    // Enable / disable Oni's status bar to replace Neovim's.
    //"statusbar.enabled": true,
    // Font size for Oni's status bar. Only valid when statusbar.enabled: true.
    //"statusbar.fontSize": "0.9em",
    //"statusbar.priority": {
    //    "oni.status.workingDirectory": 0,
    //    "oni.status.linenumber": 2,
    //    "oni.status.gitHubRepo": 0,
    //    "oni.status.mode": 1,
    //    "oni.status.filetype": 1,
    //    "oni.status.git": 3,
    //},

    // Sets the tab style in Oni. buffers will show every buffer in its own
    // tab. If you use tabs as part of your Vim workflow, you'll likely want
    // to set this to tabs, which will instead only show tabs. To disable
    // Oni's tab UI entirely and revert to the native Vim tab bar, use native.
    // To remove both the Oni and Vim tab bar use hidden. See #602 and #657
    // for discussion around this.
    //"tabs.mode": "tabs",
    // The height of the Oni UI tabs.
    //"tabs.height": "2.5em",
    // Highlights the active tab with the current mode (normal, insert,
    // visual...).
    //"tabs.highlight": true,
    // The max width of the Oni UI tabs.
    //"tabs.maxWidth": "30em",
    // When true, show a file icon in the tab.
    //"tabs.showFileIcon": true,
    // When true, show the index alongside the tab. Applies to both 'buffer'
    // and 'tab' settings for ('tabs.mode').
    //"tabs.showIndex": false,
    // If the tabs in the Oni UI should wrap or not once the screen is full.
    //"tabs.wrap": false,

    //"terminal.shellCommand": null,

    // Enable / disable the Oni animation on typing, like cursor motion when
    // typing.
    //"ui.animations.enabled": true,
    // Colorscheme to theme the UI. Sets the :colorscheme value for Vim, and
    // also loads a theme plugin, if available. Deffault onedark
    //"ui.colorscheme": "nord",
    // File icons theme to show in the tabs. They are only displayed if
    // tabs.mode is set to buffer (tab mode not yet supported). If you don't
    // want to have icons, set this option to null.
    //"ui.iconTheme": "theme-icons-seti",
    // Font family used by the UI. This currently includes the tab bar font,
    // the status bar font, and the popup menus.
    //"ui.fontFamily": "BlinkMacSystemFont, 'Lucida Grande', 'Segoe UI', Ubuntu, Cantarell, sans-serif",
    // Font size for Oni's UI. This currently includes the tab bar font, the
    // status bar font, and the popup menus.
    //"ui.fontSize": "13px",
    // Directly sets the -webkit-font-smoothing style property on the body
    // element. Available options are 'auto', 'none', 'antialiased', and
    // 'subpixel-antialiased'. More information at MDN: font-smooth
    //"ui.fontSmoothing": "auto",

    // The externalised wildmenu replaces the vim default wildmenu, that is the
    // menu that appears when pressing tab for the auto completion of options.
    // It is stylised to look more like the Quick Open and Command Palette drop
    // downs. This is only enabled if the externalised commandline is enabled.
    //"wildmenu.mode": true,

    //"workspace.defaultWorkspace": null,
    // This options sets oni to detect the workspace/ project root based on
    // markers such as a .git file or package.json,  build.xml etc. If set to
    // noworkspace Oni will only change workspace if there is none available.
    // if set to never oni will not autodetect project roots and finally if set
    // to always oni will change the project root to match the current buffer
    // open in one.
    //"workspace.autoDetectWorkspace": "noworkspace",
    //"workspace.autoDetectRootFiles": [
    //    ".git",
    //    "node_modules",
    //    ".svn",
    //    "package.json",
    //    ".hg",
    //    ".bzr",
    //    "build.xml",
    //],

    //"experimental.neovim.transport": "stdio",
    // TODO: Enable pipe transport for Windows
    //"experimental.neovim.transport": Platform.isWindows() ? "pipe" : "stdio",
    // Enable the preview of markdown files on a side panel:
    //"experimental.markdownPreview.enabled": true,
    //"experimental.particles.enabled": false,
    //"experimental.preview.enabled": false,
    //"experimental.welcome.enabled": false,

}


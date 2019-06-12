//////////////////////////////////////////////////////////////////////////////
//
// oni_config.tsx - Oni config in Typescript which is used to generaate a
// config,js file which is the REAL config Oni config file.
//
//////////////////////////////////////////////////////////////////////////////
import * as React from "react"
import * as Oni from "oni-api"

export const activate = (oni: Oni.Plugin.Api) => {
    console.log("config activated")

    // Input
    //
    // Add input bindings here:
    //
    oni.input.bind("<c-enter>", () => console.log("Control+Enter was pressed"))
    oni.input.bind("<f12>", "markdown.togglePreview")

    //
    // Or remove the default bindings here by uncommenting the below line:
    //
    // oni.input.unbind("<c-p>")
}

export const deactivate = (oni: Oni.Plugin.Api) => {
    console.log("config deactivated")
}

export const configuration = {
    //add custom config here, such as
    
    // Set whether or not Oni should check for updates on startup.
    "autoUpdate": true,

    // If the externalised commandline should be enabled or not. Setting this
    // to false will revert to the standard Vim commandline at the bottom of
    // the screen.
    "commandline.mode": false,
    // The commandline employs Icons to replace / and ? for searching, which
    // default to on. Setting this to false will remove the search icons when
    // using / or ?.
    "commandline.icons": false,
    
    // Sets additional paths for binaries. This may be necessary to
    // configure,  if using plugins or a Language Server that is not in
    // the default set of runtime paths. Note that depending on where you
    // launch Oni, there may be a different set of runtime paths sent to
    // it - you can always check byopening the developer tools and running
    // process.env.PATH in the console.
    "environment.additionalPaths": [
        "/opt/local/bin",
        "/usr/local/bin",
        "/usr/bin",
    ],

    // This determines whether the user's init.vim is loaded. This setting
    // can be a string path or a boolean. If set to true, it will load
    // init.vim from the default location. If set to a string path (ie,
    // 'C:/somedir/myinit.vim'), this will load init.vim from the specified
    // path (useful if you want a separate config between Oni & Neovim). Use
    // caution when setting this and setting oni.useDefaultConfig to true, as
    // there could be conflicts with the default configuration.
    "oni.loadInitVim": true,
    "oni.plugins.prettier": {
        "settings": {
        "semi": true,
        "tabWidth": 4,
        "useTabs": false,
        "singleQuote": false,
        "trailingComma": "es5",
        "bracketSpacing": true,
        "jsxBracketSameLine": true,
        "arrowParens": "always",
        "printWidth": 80
        },
    "formatOnSave": false,
    "enabled": false
    },
    "oni.useDefaultConfig": false,
    "ui.colorscheme": "PaperColor",
}

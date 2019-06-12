ONI is configurable via a config.js located in $HOME/.config/oni or %APPDATA%/oni on windows.

$HOME/config/oni_config.tsx is a Typescript file that is the typescript
version of the config.js.  On *nix, a symlink from
$HOME/.config/oni/config.tsx to $HOME/config/oni_config.tsx will allow
Oni->Preferences->Edit Oni config to edit the Typescript config and when saved
oni will automatically generate the javascript config file.

In VimL, the g:gui_oni variable will be set to 1, and can be validated with if exists("g:gui_oni") in VimL.

In addition to the javascript config.js bindings, you can also access most Oni functionality from your init.vim, via the OniCommand function in ex mode.
An example is:
:call OniCommand('oni.about')

## Workspace Configuration
Oni will also load a workspace configuration when opening a workspace folder. The configuration must be located at the root of the workspace, in a .oni folder.

For example, if my workspace is located at /my-project, the per-workspace configuration must be located at /my-project/.oni/config.js.

The workspace configuration takes precedence over per-user configuration.

## Oni Code Completeion
If language support is available for a language, then that language service will be queried as you type, and if there are completions available, those will be presented automatically.

Out of the box, the supported languages for rich completion are JavaScript & TypeScript. There is no special setup required for JavaScript & TypeScript language completion, but you will get best results by having a jsconfig.json or tsconfig.json at the root of your project.. You can use an empty json file with {} to get the rich completion.

<ul>
    <li><C-n> navigate to the next entry in the completion menu</li>
    <li><C-p> navigate to the previous entry in the completion menu</li>
    <li><Enter> select completion item</li>
    <li><Esc> close the completion menu</li>
</ul>

## Fuzzy Finder

<table>
  <thead>
    <tr>
      <th>Keypress</th>
      <th>Command</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>&lt;Command-p&gt;</td>
      <td>Show the Fuzzy Finder menu</td>
    </tr>
    <tr>
      <td>&lt;Ctrl-n&gt;</td>
      <td>Navigate to the next entry in the Fuzzy Finder menu</td>
    </tr>
    <tr>
      <td>&lt;Ctrl-p&gt;</td>
      <td>Navigate to the previous entry in the Fuzzy Finder menu</td>
    </tr>
    <tr>
      <td>&lt;Enter&gt;</td>
      <td>Select a Fuzzy Finder item</td>
    </tr>
    <tr>
      <td>&lt;Esc&gt;</td>
      <td>Close the Fuzzy Finder menu</td>
    </tr>
  </tbody>
</table>

## Command Palette

<table>
  <thead>
    <tr>
      <th>Keypress</th>
      <th>Command</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>&lt;Command-P&gt;</td>
      <tdShow the Command Palette</td>
    </tr>
    <tr>
      <td>&lt;Ctrl-n&gt;</td>
    
    <td>Navigate to the next entry in the Command Palette menu</td>
    </tr>
    <tr>
      <td>&lt;Ctrl-p&gt;</td>
      <td>Navigate to the previous entry in the Command Palette menu</td>
    </tr>
    <tr>
      <td>&lt;Enter&gt;</td>
      <td>Select a Command Palette item</td>
    </tr>
    <tr>
      <td>&lt;Esc&gt;</td>
      <td>Close the Command Palette menu</td>
    </tr>
  </tbody>
</table>

## Tabs

<table>
  <thead>
    <tr>
      <th>Keypress</th>
      <th>Command</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>gt</td>
      <tdCycle to the next tab</td>
    </tr>
    <tr>
      <td>gT</td>
      <td>Cycle to the previous tab</td>
    </tr>

# Shortcuts

## General

<table>
  <thead>
    <tr>
      <th>Keypress</th>
      <th>Command</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>&lt;Command-c&gt;</td>
      <td>Copy to clipboard (in visual mode)</td>
    </tr>
    <tr>
      <td>&lt;Command-v&gt;</td>
      <td>Paste from clipboard (in insert or command mode)</td>
    </tr>
    <tr>
      <td>&lt;Command-q&gt;</td>
      <td>Quit</td>
    </tr>
    <tr>
      <td>&lt;Ctrl-PageUp&gt;</td>
      <td>Cycle to next Oni process</td>
    </tr>
    <tr>
      <td>&lt;Ctrl-PageDown&gt;</td>
      <td>Cycle to previous Oni process</td>
    </tr>
  </tbody>
</table>

## Navigation

<table>
  <thead>
    <tr>
      <th>Keypress</th>
      <th>Command</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>&lt;Ctrl-G&gt;</td>
      <td>Open 'sneak mode'</td>
    </tr>
    <tr>
      <td>&lt;Ctrl-W&gt; &lt;Ctrl-V&gt;</td>
      <td>Open vertical split</td>
    </tr>
    <tr>
      <td>&lt;Ctrl-W&gt; &lt;Ctrl-S&gt;</td>
      <td>Open horizontal split</td>
    </tr>
    <tr>
      <td>&lt;Ctrl-W&gt; &lt;Ctrl-H&gt;</td>
      <td>Move left a window</td>
    </tr>
    <tr>
      <td>&lt;Ctrl-W&gt; &lt;Ctrl-L&gt;</td>
      <td>Move right a window</td>
    </tr>
    <tr>
      <td>&lt;Ctrl-W&gt; &lt;Ctrl-J&gt;</td>
      <td>Move down a window</td>
    </tr>
    <tr>
      <td>&lt;Ctrl-W&gt; &lt;Ctrl-K&gt;</td>
      <td>Move up a window</td>
    </tr>
  </tbody>
</table>

# Panes #
Cmd             | Description                            |
------          | -----------                            |
Ctrl-Shift-b    | Toggle display of the sidebar pane     |


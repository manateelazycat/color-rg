<img src="./screenshot/color-rg.png">

# What is color-rg?
color-rg is a search and refactoring tool based on ripgrep.

I'm a big fan of color-moccur.el, this extension's name is a tribute to color-moccur.el!

## Installation
Clone or download this repository (path of the folder is the `<path-to-color-rg>` used below).

In your `~/.emacs`, add the following two lines:
```Elisp
(add-to-list 'load-path "<path-to-color-rg>") ; add color-rg to your load-path
(require 'color-rg)
```

If you use Mac, you also need to install [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell)

## Usage
Bind your favorite key to functions:

| Function                                | Description                                                              |
| :--------                               | :----                                                                    |
| color-rg-search-input                   | Search user's input with current directory                               |
| color-rg-search-symbol                  | Search current symbol with current directory                             |
| color-rg-search-input-in-project        | Search user's input in project                                           |
| color-rg-search-symbol-in-project       | Search current symbol in project                                         |
| color-rg-search-symbol-in-current-file  | Search current symbol in current file                                    |
| color-rg-search-input-in-current-file   | Search user's input in current file                                      |
| color-rg-search-project-rails           | Search user's input in rails project                                     |
| color-rg-search-symbol-with-type        | Search current symbol with current directory and special file extensions |
| color-rg-search-project-with-type       | Search user's input in project and special file extensions               |
| color-rg-search-project-rails-with-type | Search user's input in rails project and special file extensions         |

### Keymap for view mode

| Key        | Description                                                             |
| :--------: | :----                                                                   |
| C-a        | Jump to first editable position of current line                         |
| Tab        | Jump to next match keyword                                              |
| Back Tab   | Jump to previous match keyword                                          |
| j          | Jump to next match keyword                                              |
| k          | Jump to previous match keyword                                          |
| h          | Jump to next match file                                                 |
| l          | Jump to previous match file                                             |
| i          | Insert current line                                                     |
| SPC        | Open file                                                               |
| RET        | Open file relative to match line                                        |
| Ctrl + m   | Open file relative to match line                                        |
| r          | Replace all matches                                                     |
| e          | Enable edit mode                                                        |
| f          | Filter results match regexp                                             |
| F          | Filter results not match regexp                                         |
| x          | Filter results match file extension                                     |
| X          | Filter results not match file extension                                 |
| u          | Don't filter file extension                                             |
| D          | Delete current line from results                                        |
| I          | Toggle to `include`/`exclude` the ignore files then rerun last search   |
| N          | Toggle to respect `color-rg-search-ignore-rules` then rerun last search |
| C          | Toggle to `smart case`/`case sensitive` then rerun last search          |
| L          | Re-search pattern as literal                                            |
| R          | Re-search pattern as regexp                                             |
| G          | Rerun last search only include specified file type                      |
| E          | Rerun last search exclude specified file type                           |
| o          | Rerun last search in (up one level) parent directory                    |
| O          | Rerun last search in project root directory                             |
| O          | Rerun last search in new directory                                      |
| s          | Rerun rg with customized directory.                                     |
| S          | Rerun rg with customized arguments.                                     |
| q          | Quit                                                                    |

### Keymap for edit mode

| Key        | Description                                     |
| :--------: | :----                                           |
| C-a        | Jump to first editable position of current line |
| C-c C-j    | Jump to next match keyword                      |
| C-c C-k    | Jump to previous match keyword                  |
| C-c C-h    | Jump to next match file                         |
| C-c C-l    | Jump to previous match file                     |
| C-c C-RET  | Open file relative to match line                |
| C-c C-v    | Disable edit mode                               |
| C-c C-d    | Delete current line                             |
| C-c C-D    | Delete all lines                                |
| C-c C-r    | Recover current line                            |
| C-c C-R    | Recover buffer content                          |
| C-c C-q    | Quit                                            |
| C-c C-c    | Apply changed line to files                     |

### Work with isearch
Add this into your emacs config file:
```
(define-key isearch-mode-map (kbd "M-s M-s") 'isearch-toggle-color-rg)
```
When using `isearch-forward', type "M-s M-s" to search current isearch string with color-rg.

### Research with new files
`color-rg-rerun-change-files` can limit search files with
GLOB. This function used rg arguments '--type', '--add-type'.

Default search files is "everything" which means use rg without '--type' argument.

If search files is "all", search rg like "rg --type all".

Other complete candidates are mostly read from "rg --type-list"
command, which is predefined in rg.

Of course you can specify your own GLOB, just insert them as you
like. For example, if you input a GLOB like `*mypersonalglob*`, which
match none of the candidates, then, color-rg will call shell command
like:
```
"rg --add-type 'custom:*mypersonalglob*' --type custom ..."
```

### Research with customized arguments
If none of the preset meet your requirements,
`color-rg-customized-search` can release the full power of `rg`.

When `color-rg-customized-search` is called,
the full command of rg will show up, and you can add any argument to rg as in shell.

you must escape special characters when using `color-rg-customized-search`,
for example:

```
rg --type-add custom\:\*.el
# or
rg --type-add 'custom:*.el'
# or
rg --type-add "custom:*.el"
```

## Contributor
<a href = "https://github.com/manateelazycat/color-rg/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=manateelazycat/color-rg"/>
</a>

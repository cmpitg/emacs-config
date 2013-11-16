# cmpitg's Emacs config

## Introduction

This is my (cmpitg's) personal Emacs 24 configuration.  This configuration is
packaged so it could be automagically installed in any new system which runs
Emacs 24+.  The best and most recommended way to use is via the installation
method mentioned below.  You could use this configuration in parallel with any
Emacs configuration (such as
[Emacs Prelude](https://github.com/bbatsov/prelude)) as long as other
configurations don't mess up with the `~/emacs-config` directory.

## Screenshot

![My Emacs screenshot](http://i.imgur.com/FNnCESN.png "My Emacs setup")

## Requirements

* A \*nix system, I *don't and probably never will* support Window$

* Emacs 24+

* [Xiki](http://xiki.org) (optional)

* For the GUI file browser:

  - Emacs is running under server mode with default socket path.  This could
    be achieved by using the `init.el` config or simply evaluating
    `(server-start)` in your Emacs.

  - PySide for Python 3 (`python3-pyside` package in a Debian-based systems)

* For file opening: zenity (GTK+ GUI dialog, used for file choosing)

* For `ibus-mode`: Python-Xlib

* For Python development:
  - Packages `jedi epc` for auto-completion

* For Ruby development:
  - [RSense](http://cx4a.org/software/rsense/) for Ruby completion
  - Gems: `pry pry-doc yard` for Ruby-dev and [Pry](http://pryrepl.org/) integration

### Installing requirements in Debian

Im my system, I install Ruby using [RVM](https://rvm.io/), use Python 3.2, and
switch from `su` to `sudo`.  So the installation process is roughly:

```sh
# Install Emacs 24+, I built it manually
sudo aptitude install python3-pyside python-xlib zenity
sudo pip-3.2 install python-xlib jedi epc
gem install -V pry pry-doc yard
# Install Xiki, refer to https://github.com/trogdoro/xiki
```

## Installation

* First, clone the repository to your `$HOME`:

  ```sh
  cd ~
  git clone git://github.com/cmpitg/emacs-config.git
  ```

* Make sure you have a local `bin` directory for executable files.  You can
  skip this step if you've already had:

  ```sh
  mkdir -p ~/bin/
  echo "export PATH=$HOME/bin:$PATH" >> ~/.bashrc
  # For non-Bash users, add the same thing to your rc, e.g. with Zsh:
  # echo "export PATH=$HOME/bin:$PATH" >> ~/.zshrc
  ```
* Now, symlink all the executables to your `$HOME/bin`:

  ```sh
  ln -s ~/emacs-config/bin/filebrowser-emacs.py ~/bin/
  ln -s ~/emacs-config/bin/te ~/bin/
  ln -s ~/emacs-config/bin/emacs-xiki ~/bin/
  ln -s ~/emacs-config/rsense/bin/rsense ~/bin/
  ```

* Create `~/.rsense` for RSense:

  ```sh
  ~/emacs-config/bin/rsense-init
  ```

* Disable Pry's pager (for Emacs-Pry integration):

  ```sh
  echo "Pry.config.pager = false" >> ~/.pryrc
  ```

* Edit Xiki path in `~/emacs-config/emacs-xiki.el`

* Make your own customization file if necessary, edit and have fun:

  ```sh
  touch ~/emacs-config/emacs-custom.el
  ```

(TODO) Desktop file

## Update

```sh
cd ~/emacs-config/
git pull
```

Then restart Emacs.  Note that if you have made some changes to files other
than `emacs-custom.el`, `git rebase` is better than `git pull`.  Consult Stack
Overflow for
[the reason why](http://stackoverflow.com/questions/3357122/git-pull-vs-git-fetch-git-rebase).

## Uninstallation

Just remove the `~/emacs-config` directory and your `~/emacs-custom.el` if necessary:

```sh
rm -rf ~/emacs-config
rm ~/emacs-custom.el
```

And the executable files:

```sh
rm ~/bin/{te,filebrowser-emacs.py,emacs-xiki}
```

## Running

* Run the `te` (or `~/bin/te`) command.  All other customization must go into
  your `~/emacs-custom.el` file.

* If you're using Xiki, install Xiki and run `emacs-xiki`.

* By default, the variable `$RSENSE_HOME` is set to
  `$HOME/emacs-config/rsense`.  To change this, search for `(setenv
  "$RSENSE_HOME"` in `the `.el` file(s) and change it as you need.

* Emacs uses [Yasnippet](https://github.com/capitaomorte/yasnippet) for
  [Textmate-like](https://macromates.com) snippet feature.  Put your custom
  snippets in your `*snippet-dir*` directory.  By default, `*snippet-dir*` is
  `~/emacs-config/snippets` and can be changed in `.el` files.

## Detailed Description

Differences from default Emacs:

* [ErgoEmacs](http://ergoemacs.org/) keybindings, with `<Super>` key as the
  modifier.

* Optimized for
  [Programmer Dvorak key layout](http://www.kaufmann.no/roland/dvorak/index.html).

* Making extensive use of mouse and `<Super>` key.

* "When in doubt, leave it out".  Use only what I need.

* Opening file with a GUI dialog (provided by Zenity, defined in
  `~/emacs-config/emacs-cmpitg-config/custom-functions.el`).

* Maintainable.

* Automatically `chmod +x` shebang-ed files.

## Features

* Files and directories are displayed as a tree:
  - Double click an item it in your Emacs server
  - Right click an item to open the context menu:
    + Browse the current item if it's a directory
    + Copy full path
    + Copy file/directory name
    + Delete file/directory from your drive

* Current visited path is displayed in the first text box called the
  *pathbar*.  Supported format includes `~` and shell variables (such as
  `$HOME`, `$SOMEDIR`).  Shortcut to go directly to the *pathbar*: `Ctrl+L`.

* User could modify directly the *pathbar* or click on the left-top browse
  button to visit a directory.  Shortcut to open directory browser dialog:
  `Ctrl+O`.

* The displayed files/directories could be filter by name with the regular
  expression in the second text box (called *filterbar*).  Shortcut to go
  directly to the *filterbar*: `Ctrl-F`.

* All temporary buffers (buffers started and ended with `*`, such as `*Help*`,
  `*Completions*`, `*Messages*`, ...) are treated as popup buffer using
  [popwin](https://github.com/m2ym/popwin-el) extension.

## Use Cases With Keybindings

TODO: Making nice table with: Keybinding - Description - Function - Provided by

* Use **mouse effectively**, [Acme mouse chords](http://acme.cat-v.org/mouse):
  - Selecting text by dragging *button one*, keep holding down *button one*:
    * Press *button two* to cut
    * Press *button three* to paste
  - When some text is selected, press *button three* to search for selection
    in the current buffer.

  ![Acme mouse chords in Emacs](http://i.imgur.com/H0Xh8RG.png)

  Original image is at [acme.cat-v](http://acme.cat-v.org/mouse).

* What does this keybinding do? `C-h k [keybinding]`

* Description for this function and its keybinding? `C-h f [function-name]`

* Scratch/temporary buffer:
  - Switch default scratch buffer: `C-f1`
  - New scratch buffer: `C-x C-n`

* Open file:
  - Open file with Zenity: `f3`
  - Open file with Helm: `<M-f3>`:
    * `C-l` to go up one level
    * `C-z` to go down one level
  - Open current file as root, using `sudo`: `s-z`
  - Close current buffer: `<C-f4>`

* Bookmarks:
  - Go to bookmark: `S-f8`
  - List bookmarks: `C-x r l`
    * `d` to delete bookmark
    * `x` to commit deletions
  - Add to bookmark: `C-x r m`

* Window management:
  - Delete all other windows: `C-%`
  - Hide current window: `<S-f4>`
  - Split vertically: `C-7`
  - Split horizontally: `C-5`
  - Hide [popup](https://github.com/m2ym/popwin-el) window: `C-g`
  - Scroll other window down: `C-M-v`
  - Scroll other window up: `C-S-M-v`

* Buffer management:
  - Show current buffer list: `f8`
  - Kill current buffer: `C-delete` or `C-f4`

* Basic text processing:
  - Upcase word, lowcase word, and capitalize word: `M-u`, `M-l`, and `M-c`

### With Any Interactive mode

The following keybindings are applied to when you want to interative with a
REPL.

Currently supported REPL:

* [MozRepl](https://github.com/bard/mozrepl/wiki) (for JavaScript)
* [Pry](http://pryrepl.org/) (for Ruby).
* [Geiser](http://www.nongnu.org/geiser/) for Scheme and
  [Racket development](http://docs.racket-lang.org/guide/Emacs.html).
* Python (built-in).

General keybindings:

* Invoke and/or jump to REPL: `C-c C-z` or `C-c C-i`
* Eval last expression: `C-x C-e` or `C-c C-e`
* Eval region: `C-c C-r`
* Eval buffer: `C-c C-b` or `C-c C-c` (in some modes)
* Eval function: `C-M-x` or `C-c C-c` (in some modes)
* Show documentation of current word/symbol/identifier: `f1`

#### With Python Development

(TODO) Making screencast, solving a Project Euler problem

#### With Racket Development

(TODO) Making screencast, solving a Project Euler problem

#### File Management with Sunrise Commander

* Open Sunrise: `s-SPC SPC`
* Sunrise change dir: `s-SPC c`
* In Dired mode, to toggle detail information: `(` or `)`

In Sunrise mode:

* Hide details: `C-backspace`

(TODO) Making screencast with use cases:

* Mass renaming with `wdired`
* Mass copy/moving
* Shell commands
* Open file with external program
* Tree browsing

#### Working with Git

Cheatsheet: http://daemianmack.com/magit-cheatsheet.html

Git should be setup with SSH.

Git status: `s-SPC g`

- Quit: `q`

- Committing:
  * Previous hunk, next hunk: `p`, `n`
  * Stage/unstage current hunk: `s`/`u`
  * Stage/unstage all hunks: `S`/`U`
  * Ignore file: `i`
  * Toggle visibility: `tab`
  * Toggle visibility of all: `S-tab`
  * Reload buffer: `g`
  
- Commit: `c`
  * Execute commit: `C-c C-c`
  
- History and verbose history: `l`, `L`

- Copy SHA1: `C-w`

- Marking:
  * Mark/unmark current commit: `..`/`C-u ..`
  * Toggle commit marking: `.`

- Diff-ing:
  *Show diff between marked and current commit: `=`

- Reseting:
  * Current head: `x`
  * Hard reset, **destructive**: `X`

- Pushing & pulling & rebasing:
  * Push: `P`
  * Pull: `F`
  * Rebase: `R`

- Branching:
  * Switch branch: `b`
  * Create and switch branch: `B`

#### Github

* `gist-region`

### File Browser

The file browser is written in `PySide` (Python wrapper for Qt framework).
Source code is the file browser is distributed under the terms of the GNU
General Public License version 3.0.

## TODOs

Refactor this document into simple use cases/tasks.

* `s-v` to go to package manager's package list (`package-list-packages`)

* `s-\` to toggle `ibus-mode` (`ibus-mode`), then `C-M-S-SPC` to toggle Ibus (`$toggle-ibus`)

* Code:
  - `C--` to toggle comment on selection (`'$toggle-comment-region`)

* Toggle whitespace visibility `C-<menu> C-w`:
  - Delete redundant whitespaces `s-w`

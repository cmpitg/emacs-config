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

![My Emacs screenshot](https://raw.github.com/cmpitg/emacs-config/master/images/2013-11-01_08-59-30_Emacs_session.png "My Emacs setup")

## Requirements

* A \*nix system, I *don't and probably never will* support Window$

* Emacs 24+

* [Xiki](http://xiki.org) (optional, if you use Xiki)

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
# Install Emacs 24+, I built in manually
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
* Now, symlinks all the executables to your `$HOME/bin`:

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
    touch ~/emacs-custom.el
    ```

(TODO) Desktop file

## Update

(TODO)

    ```sh
    cd ~/emacs-config/
    git pull
    ```

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

* [ErgoEmacs](http://ergoemacs.org/) keybindings, with `<Super>` key as the modifier.

* Making extensive use of mouse and `<Super>` key.

* "When in doubt, leave it out".  Use only what I need.

* Opening file with a GUI dialog (provided by Zenity, defined in
  `~/emacs-config/emacs-cmpitg-config/custom-functions.el`).

* Maintainable.

### Keybindings and use cases

* What does this keybinding do? `C-h k [keybinding]`

* Description for this function and its keybinding? `C-h f [function-name]`

* Open file:
  - `<f3>` open file with Zenity
  - `<M-f3>` open file with Helm

* Bookmarks:
  - Go to bookmark: `<S-f8>`
  - List bookmarks: `C-x r l`
    * `d` to delete bookmark
    * `x` to commit deletions
  - Add to bookmark: `C-x r m`

#### With any interactive mode

The following keybindings are applied to when you want to interative with a
REPL.  Currently supported REPL:
[MozRepl](https://github.com/bard/mozrepl/wiki) (for JavaScript) and
[Pry](http://pryrepl.org/) (for Ruby).

* Invoke and jump the REPL: `C-c C-i`

* Eval last expression: `C-x C-e` or `C-c C-e`

* Eval region: `C-c C-r`

* Eval buffer: `C-c C-b`

* Eval function: `C-M-x` or `C-c C-c`

### File Browser

The file browser is written in `PySide` (Python wrapper for Qt framework).
Source code is the file browser is distributed under the terms of the GNU
General Public License version 3.0.

#### All features:

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

## TODOs

Refactor this document into simple use cases/tasks.

## Fragemented Notes

* Use **mouse effectively**

* Open/save file:
  - `<f3>` to open file with GUI (Zenity) (`$open-file-gui`)
  - `<S-f3>` to open file with GUI (Zenity) in another window (`$open-file-gui-other-window`)
  - `<f2>` to save file (`$save-file`)
  - `s-z` to open current file as root using `sudo` (`$open-current-file-as-admin`)

* `s-v` to go to package manager's package list (`package-list-packages`)

* `s-\` to toggle `ibus-mode` (`ibus-mode`), then `C-M-S-SPC` to toggle Ibus (`$toggle-ibus`)

* `<C-down-mouse-1>` to show buffer menu (`mouse-buffer-menu`)

* `<S-mouse-1>` to execute shell command on selection then pipe to the current position (`$exec-then-pipe-selection`)

* `<mouse-2>` (middle click) to eval selection (`$eval-selection`)

* `<M-mouse-3>` to select and do that again to kill (`mouse-secondary-save-then-kill`)

* `<C-f4>` to close the current file (`$close-file`)

* `<f8>` to toggle the Speedbar buffer (`speedbar-toggle`):
  - `g` to update its contents
  - `f` to display directory
  - `b` to display current buffers

* Window splitting:
  - `C-%` to make full one window (`delete-other-windows`)
  - `C-7` and `<M-mouse-2>` to split window vertically (`split-window-vertically`)
  - `C-5` to split window horizontally (`split-window-horizontally`)
  - `<S-f4>` and `<S-mouse-2>` to delete window (`delete-window`)

* Getting help:
  - `<f1>` to man current word (`$man-this`)
  - `C-h k` to determine if a key sequence is of any binding (`describe-key`)
  - `C-h f` to get a function definition (`describe-function`)

* Code:
  - `C-/` to toggle comment on selection (`'$toggle-comment-region`) (TODO)

* Toggle whitespace visibility `C-<menu> C-w`:
  - Delete redundant whitespaces `s-w`

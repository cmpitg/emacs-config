# cmpitg's Emacs config

## Requirements

* Emacs 24

* zenity (GTK+ GUI dialog, used for file choosing)

* PySide for Python 3 (`python3-pyside` package in a Debian-based systems)

* Python-Xlib is required for `ibus-mode` to work

## Installation

(TODO)

    ```sh
    cd ~
    git clone git://github.com/CMPITG/emacs-config.git
    mkdir -p ~/bin/
    echo "export PATH=$HOME/bin:$PATH" >> /etc/.bashrc
    # For non-Bash users, add the same thing to your rc, e.g. with Zsh:
    # echo "export PATH=$HOME/bin:$PATH" >> /etc/.zshrc
    cp -Rv ~/emacs-config/bin/* ~/bin/
    chmod +x ~/bin/te ~/bin/filebrowser-emacs.py
    # TODO: Desktop file
    ```

## Update

(TODO)

    ```sh
    cd ~/emacs-config/
    git pull
    ```

## Running

Run the `te` command (TODO).

## Description

Differences from default Emacs:

* [ErgoEmacs](http://ergoemacs.org/) keybindings, with `<Super>` key as the modifier.

* Making extensive use of mouse and `<Super>` key.

* "When in doubt, leave it out".  Use only what I need.

* Opening file with a GUI dialog (provided by Zenity, defined in `~/emacs-config/emacs-cmpitg-config//custom-functions.el`)

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

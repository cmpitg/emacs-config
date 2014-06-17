;;
;; Copyright (C) 2012-2013 Duong H. Nguyen <cmpitgATgmaildotcom>
;;
;; This project is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This project is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;;
;; Theming and stuff
;;

(require 'color-theme)

;; Comment color
;; (set-face-foreground 'font-lock-comment-face "#3a345f")
(set-face-attribute 'font-lock-comment-face nil :foreground "#A79B9F")

;; Set cursor color
;; (set-cursor-color "cyan")
;; (set-cursor-color "gray")
(set-cursor-color "black")
;;(set-background-color "#f2f2f2")
(set-background-color "#efefef")

;;(load "~/emacs-config/themes/color-theme-textmate-modified.el")
;;(require 'color-theme-textmate-modified)
;;(color-theme-textmate-modified)

(setq search-highlight 1)

;; More tolerable stack
(setq max-lisp-eval-depth 15000
      max-specpdl-size    15000)

;; Never change case when replacing
(setq-default case-replace nil)

;;; fill-column
(setq-default fill-column 78)
(set-fill-column 78)

;; Disable backup file
(setq make-backup-files nil)

;;; Auto complete switching buffer mode
(iswitchb-mode t)

;; Default scratch-buffer mode
(setq initial-major-mode 'emacs-lisp-mode)
(setq-default initial-major-mode 'emacs-lisp-mode)

;; Don't let the cursor go into minibuffer prompt
;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

;; Save cursor position each time you open a file
;; http://ergoemacs.org/emacs/emacs_save_cursor_position.html
(require 'saveplace)
(setq save-place-file "~/emacs-save-places")
(setq-default save-place t)

;; yes/no questions become y/n questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Backspace and Del delete selection, except in paredit-mode
(delete-selection-mode 1)

;; Set the default tab width
(setq default-tab-width 4)

;; Set tab width
(setq tab-width 4)

;; Default tab-size for C
(setq-default c-basic-offset 4)

;; Expand tabs to spaces
(setq-default indent-tabs-mode nil)

;; Show column number
(column-number-mode 1)

;; Enable recent files
(recentf-mode 1)

;; Highlight the editing line
(hl-line-mode 1)

;; Turn on the search-highlighting feature
(setq search-highlight 1)

;; Hide the toolbar
(tool-bar-mode -1)

;; Hide the scrollbar
(scroll-bar-mode -1)

;; Hide the menu bar
;; (menu-bar-mode -1)
(menu-bar-mode 1)

;; Case-insensitive searching
(setq-default case-fold-search t)
(setq case-fold-search t)

;; ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)

;; dim the ignored part of the file name
(file-name-shadow-mode 1)

;; minibuffer window expands vertically as necessary to hold the text
;; that you put in the minibuffer
(setq resize-mini-windows t)

;; Never change case when replacing
(setq-default case-replace nil)

;; Display time
(display-time)

;; Turn off welcome message
(setq inhibit-startup-message t)

;; Display the size of the buffer
(size-indication-mode 1)

;; Numbering lines
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%d ")

;; Show the battery indicator
(display-battery-mode 1)

;;;; Use font lock
(global-font-lock-mode t)
(setq font-lock-maximum-size nil)

;;; fill-column
(setq-default fill-column 78)
(set-fill-column 78)

(require 'desktop)
(require 'tar-mode)

;; Pretty diff mode
(autoload 'ediff-buffers "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files-remote "ediff"
  "Intelligent Emacs interface to diff")

;; Disable backup file
(setq make-backup-files nil)

;; Set transparency
;; (set-frame-parameter nil 'alpha 78)

;; Set mark-ring-max
(setq mark-ring-max 512)

;; Disable shift selection
(setq shift-select-mode nil)

;; Echo when trying to kill in a read-only buffer
(setq kill-read-only-ok t)

;;; Blink cursor
(blink-cursor-mode t)

;; Always suggest-key-bindings
(setq suggest-key-bindings t)

;; Set ispell-dictionary
(ispell-change-dictionary "en_US")

;; grep command
(setq grep-command "grep -i -nH -e ")

;; Set printing type
(setq ps-paper-type 'a4)

;;; Use the same clipboard with X
(setq x-select-enable-clipboard t)

;;; Auto complete switching buffer mode
(iswitchb-mode t)

;;; Set frame title
(setq frame-title-format
      '(multiple-frames "%b" ("@" system-name )))

;;; Custom dired
(setq dired-listing-switches "-lahF")

;; set font
(set-face-attribute 'default nil
  :height 110
  :family "Monaco")


;;; Custom unique naming method
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

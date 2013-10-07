; Load my favourite keybinding options
;(load "~/emacs-config/emacs-cmpitg-config/aliases.el")
;; (load "~/emacs-config/emacs-cmpitg-config/custom-functions.el")
;; (load "~/emacs-config/emacs-cmpitg-config/keymap-ergo.el")
;; (load "~/emacs-config/emacs-cmpitg-config/keymap-common.el")

(load "~/emacs-config/emacs-prexiki.el")
(setq *xiki-path*
      "/home/cmpitg/.rvm/gems/ruby-1.9.3-p448/gems/trogdoro-el4r-1.0.9/data/emacs/site-lisp/")

(global-unset-key (kbd "C-/"))

; Load el4r, which loads Xiki
(add-to-list 'load-path *xiki-path*)
(require 'el4r)
(el4r-boot)

; Fix open-file command
(global-set-key (kbd "C-o") '$open-line)

; Let me press "y", instead of type "yes"
(fset 'yes-or-no-p 'y-or-n-p)

; C-k kills whole line if at beginning
(setq kill-whole-line nil)

; Make tabe into spaces when you type them
(setq-default indent-tabs-mode nil)
; Display existing tabs as 2 characters wide
(setq-default tab-width 2)

; No new frame for ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
; Show ediff views side-by-side
(setq ediff-split-window-function 'split-window-horizontally)

; Set default font
(set-face-attribute 'default nil
  :height 110
  :family "Monaco"
  )

; Save cursor location upon closing files, and restore upon reopening
(require 'saveplace)
(setq-default save-place t)

; Don't create #... files when editing
(setq make-backup-files nil)

;; Numbering lines
(require 'linum)
(global-linum-mode 1)

;;; fill-column
(set-fill-column 78)

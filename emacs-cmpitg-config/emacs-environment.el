;; read-quoted-char-radix (10 or 16)

;; Set the appropriate frame size
; (set-frame-size-according-to-resolution)

;; ;; Theme
;; (load "~/emacs-config/themes/color-theme-blackboard.el")
;; (load "~/emacs-config/themes/color-theme-molokai/color-theme-molokai.el")
;; (require 'color-theme-molokai)

;; (load "~/emacs-config/themes/color-theme-inspiration-dark.el")
;; (require 'inspiration)

;; (load "~/src/elisp/twilight-emacs/color-theme-twilight.el")

;; (add-to-list 'load-path "~/src/elisp/zenburn-emacs")
;; (require 'color-theme-zenburn)

;(load "~/src/elisp/emacs24-solarized/solarized-theme.el")
;(load "~/src/elisp/emacs24-solarized/solarized-light-theme.el")
;(color-theme-molokai)
;(color-theme-zenburn)
;(color-theme-textmate)
;(color-theme-twilight)
;(inspiration-144382)
;(inspiration-648409)
;(inspiration-990434)
;(color-theme-charcoal-black)
;(color-theme-calm-forest)

;; Using system font

(setq font-use-system-font t)

;; Custom variables

(custom-set-variables
 ;; '(column-number-mode t)
 ;; '(display-battery-mode t)
 ;; '(display-time-mode t)
 ;; '(size-indication-mode t)
 '(face-font-family-alternatives (quote (("Monaco" "Consolas" "Monospace")
                                         ("Monaco" "Consolas" "CMU Typewriter Text" "fixed")
                                         ("Geneva" "Sans Serif" "helv" "helvetica" "arial" "fixed")
                                         ("helv" "helvetica" "arial" "fixed"))))
 '(safe-local-variable-values (quote ((Syntax . ANSI-Common-Lisp) (Base . 10) (encoding . utf-8))))
 '(show-paren-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;; Custom fonts

(custom-set-faces
 '(default ((t (:inherit nil
                         :stipple nil
                         :inverse-video nil
                         :box nil
                         :strike-through nil
                         :overline nil
                         :underline nil
                         :slant normal
                         :weight normal
                         ;; :height 98
                         :width normal
                         :foundry "unknown"
                         :family "Monaco"))))
 ;; '(mode-line ((t (:background "grey75"
 ;;                              :foreground "#3d3d3d"
 ;;                              :inverse-video t
 ;;                              :box (:line-width 1 :color "#000000" :style released-button)
 ;;                              :slant normal
 ;;                              :weight normal
 ;;                              :height 100
 ;;                              :family "Geneva"))))
 ;; '(mode-line ((t (:inverse-video t
 ;;                                 :slant normal
 ;;                                 :weight normal
 ;;                                 :height 100
 ;;                                 :family "Geneva"))))

 '(rst-level-1-face ((t (:embolden t))) t))

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

;; Show line number
(line-number-mode 1)
;; Set linumber format
(setq linum-format "%d ")

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

;; Show the battery indicator
(display-battery-mode 1)

;;;; Use font lock
(global-font-lock-mode t)
(setq font-lock-maximum-size nil)

;;; fill-column
(setq-default fill-column 78)
(set-fill-column 78)

;;; Automatically turn on auto-fill and refill mode
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'text-mode-hook (lambda () (refill-mode 1)))

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

;; Change cursor type
;;(set-default 'cursor-type 'hbar)
(set-default 'cursor-type 'bar)
;;(set-default 'cursor-type 'box)

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

;;; Custom unique naming method
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

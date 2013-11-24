;; -*- mode: emacs-lisp -*-

;;
;; Copyright (C) 2012-2013 Duong H. Nguyen <cmpitgATgmaildotcom>
;;
;; bogoengine is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; bogoengine is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;;;
;;; TODO
;;;
;;; * write documentation/make screencast about workflow
;;; * make "~/emacs-config" a constant    
;;; * Rename this file
;;; * Global variables are placed in a seperated file
;;; * Load everything with `eval-after-load` so config would be messed up if a
;;;   package is not loaded
;;; * Better interface to ELPA and el-get:
;;;   - Check if a package exists
;;;   - Check for package dependencies
;;;   - Update package DB
;;;   - Automatically check for updates
;;;   - Update packages
;;;   - Async download and installation with progress bar (later-do)
;;; * [task] Decouple: defining & implementing (global vars at one place)
;;; * Function: better M-q inside comments
;;; * Modularize all of my customization:
;;;   - Each feature is self-documented, using TomDoc, with HTML output hosted
;;;     on my site
;;;   - (require 'a-feature) to load
;;; * [doc] Documentation and cheatsheet for all custom functions, with HTML
;;;   output for online browsing and inline lookup
;;; * [doc] Guide on how to package
;;; * [doc] Guide on how to write a mode
;;; * [task] Structural editing (suggestion based on the context, at a
;;;   separated window, mouse click support, easy manipulation)
;;; * Sublime-like preview buffer
;;;

;; Global constants

(setenv "$RSENSE_HOME" (expand-file-name "~/emacs-config/rsense"))

(setq openwith-associations
      '(("\\.pdf\\'" "evince" (file))
        ("\\.mp3\\'" "smplayer" (file))
        ("\\.odt\\'" "libreoffice" (file))
        ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mp4\\|m4v\\)\\'" "smplayer" (file))
        ("\\.\\(?:jp?g\\|png\\)\\'" "eog" (file))))

(setq *custom-functions-path*  "~/emacs-config/emacs-cmpitg-config/custom-functions.el"
      *snippet-dir*            "~/emacs-config/snippets"
      *custom-els-dir*         "~/emacs-config/emacs-cmpitg-config/"
      *default-lisp-repl-path* (expand-file-name "~/bin/sbcl")
      *elpa-package-dir*       "~/.emacs.d/elpa/")

;; cmpitg's specific configuration

(load-file *custom-functions-path*)

($load-custom-el "keymap-common.el"
                 "aliases.el")

;;
;; Start Emacs server
;;

($server-start)

;;
;; Package manager
;;

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("SC" . "http://joseito.republika.pl/sunrise-commander/"))

(package-initialize)

;; Fetch the list of available packages
(when (not package-archive-contents)
  (package-refresh-contents))

(apply #'$install-packages *package-list*)

;; Add all the load path

(mapc #'(lambda (dir)
          (add-to-list 'load-path (format "%s%s" *elpa-package-dir* dir)))
      (directory-files *elpa-package-dir* nil ".*"))

;;
;; el-get - yet another sophisticated package manager
;;
;; https://github.com/dimitri/el-get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  ($install-or-update-el-get))

(add-to-list 'el-get-recipe-path "~/emacs-config/el-get-user/recipes")
(el-get 'sync)

(apply #'el-get-install *el-get-package-list*)

;;
;; Setting the environment
;;

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

;;
;; hexrgb for color manipulation
;;

(require 'hexrgb)

;;
;; File management with dired
;;
;; Default: -lahF

(require 'dired-details+)

(setq dired-listing-switches "-lhFgG --group-directories-first")

;;
;; Theming and stuff
;;

(require 'color-theme)

;; Comment color
;; (set-face-foreground 'font-lock-comment-face "#3a345f")
(set-face-attribute 'font-lock-comment-face nil :foreground "#3a345f")

;; Set cursor color
;; (set-cursor-color "cyan")
;; (set-cursor-color "gray")
(set-cursor-color "black")
;; (set-background-color "#f2f2f2")
(set-background-color "#efefef")

;;(load "~/emacs-config/themes/color-theme-textmate-modified.el")
;;(require 'color-theme-textmate-modified)
;;(color-theme-textmate-modified)

;;
;; Powerful and beautiful modeline
;;
;; Repo (bad): https://github.com/milkypostman/powerline
;; Another (better): https://github.com/jonathanchu/emacs-powerline

($add-load-path "~/emacs-config/emacs-local-packages/emacs-powerline/")
(require 'cl)
(require 'powerline)

;;
;; Popwin - better popup management
;;
;; https://github.com/m2ym/popwin-el
;;
;; | Key    | Command                               |
;; |--------+---------------------------------------|
;; | b      | popwin:popup-buffer                   |
;; | l      | popwin:popup-last-buffer              |
;; | o      | popwin:display-buffer                 |
;; | C-b    | popwin:switch-to-last-buffer          |
;; | C-p    | popwin:original-pop-to-last-buffer    |
;; | C-o    | popwin:original-display-last-buffer   |
;; | SPC    | popwin:select-popup-window            |
;; | s      | popwin:stick-popup-window             |
;; | 0      | popwin:close-popup-window             |
;; | f, C-f | popwin:find-file                      |
;; | e      | popwin:messages                       |
;; | C-u    | popwin:universal-display              |
;; | 1      | popwin:one-window                     |

(require 'popwin)
(popwin-mode 1)

(push '("\*anything*" :regexp t :height 20)         popwin:special-display-config)

(setq anything-samewindow nil)

(push '("*anything*" :height 20)                    popwin:special-display-config)

(push '(dired-mode :position top)                   popwin:special-display-config)

(push "*Backtrace*"                                 popwin:special-display-config)
(push "*Shell Command Output*"                      popwin:special-display-config)
(push '(compilation-mode :noselect t)               popwin:special-display-config)

;; slime
(push "*slime-apropos*"                             popwin:special-display-config)
(push "*slime-macroexpansion*"                      popwin:special-display-config)
(push "*slime-description*"                         popwin:special-display-config)
(push '("*slime-compilation*" :noselect t)          popwin:special-display-config)
(push "*slime-xref*"                                popwin:special-display-config)
(push '(sldb-mode :stick t)                         popwin:special-display-config)
(push 'slime-repl-mode                              popwin:special-display-config)
(push 'slime-connection-list-mode                   popwin:special-display-config)

;; vc
(push "*vc-diff*"                                   popwin:special-display-config)
(push "*vc-change-log*"                             popwin:special-display-config)

;; undo-tree
(push '(" *undo-tree*" :width 0.3 :position right)  popwin:special-display-config)

;;
;; Tab bar
;;

;; (setq tabbar-ruler-global-tabbar t)    ; If you want tabbar
;; ;; (setq tabbar-ruler-global-ruler t)     ; if you want a global ruler
;; ;; (setq tabbar-ruler-popup-menu t)       ; If you want a popup menu.
;; ;; (setq tabbar-ruler-popup-toolbar t)    ; If you want a popup toolbar
;; ;; (setq tabbar-ruler-popup-scrollbar t)  ; If you want to only show the scroll
;; ;;                                        ; bar when your mouse is moving.
;; (require 'tabbar-ruler)

;;
;; Smartscan helps jumping between occurrences of a symbol
;;

(require 'smartscan)
(global-smartscan-mode 1)

;; `smartscan-symbol-go-forward' - M-n
;; `smartscan-symbol-go-backward' - M-p

;;
;; Highlight brackets
;;

;; Somehow show-paren-mode is slow, using rainbow-delimiters instead
;; (show-paren-mode 1)
;; (setq show-paren-delay 0)
;; ;; (setq show-paren-style 'expression)
;; (setq show-paren-style 'parenthesis)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;
;; Sunrise commander
;;

(require 'sunrise-commander)
(require 'sunrise-x-loop)
(require 'sunrise-x-tabs)
(require 'sunrise-x-tree)
(require 'sunrise-x-modeline)

(setq sr-listing-switches " --time-style=locale --group-directories-first -alDhgG")

;; Display modeline using UTF8 characters
(setq sr-modeline-use-utf8-marks t)

;;
;; Magit - the excellent Git mode for Emacs
;;

;; Home:
;;   http://magit.github.io/
;; Cheatsheet:
;;   http://daemianmack.com/magit-cheatsheet.html
;; Manual:
;;   http://magit.github.io/magit/magit.html

(require 'magit)

;;
;; Monky - Magit for Mercurial
;;

(require 'monky)

;;
;; Acme-like mouse chord
;;
;; https://github.com/akrito/acme-mouse.git

(load "~/emacs-config/emacs-local-packages/acme-mouse/acme-mouse.el")

;;
;; Open with file with external application
;;

(require 'openwith)
(openwith-mode t)

;;
;; dash - modern Emacs Lisp APIs
;;

;; https://github.com/magnars/dash.el

(require 'dash)
(eval-after-load "dash" '(dash-enable-font-lock))

;;
;; JSON mode
;;

(require 'json)

;;
;; Expand region - marking based-on semantic
;;

(require 'expand-region)

;;
;; Redo mode
;;

;; (require 'redo+ nil 'error)

;;
;; Gist
;;

;; https://github.com/defunkt/gist.el

(require 'gist)

;;
;; Smooth scrolling
;;

(require 'smooth-scrolling)

;;
;; JavaScript
;;

;; (require 'js3-mode)
;; (setq js3-auto-indent-p t
;;       js3-enter-indents-newline t
;;       js3-indent-on-enter-key t)

(require 'js2-mode)
(add-hook 'js-mode-hook 'js2-minor-mode)
($auto-load-mode '("\\.js\\'") 'js2-mode)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(add-hook 'html-mode-hook '$auto-reload-firefox-after-save-hook)
(add-hook 'css-mode-hook '$auto-reload-firefox-after-save-hook)

;;
;; Firefox integration setup
;;

($add-load-path "~/emacs-config/emacs-local-packages/moz/")
(require 'moz)
(add-hook 'javascript-mode-hook '$setup-moz-javascript)
(add-hook 'js3-mode-hook '$setup-moz-javascript)

;;
;; Predictive abbreviation
;;

;; (require 'pabbrev)
;; (global-pabbrev-mode)

;;
;; Slime for Common Lisp development
;;

(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(setq inferior-lisp-program *default-lisp-repl-path*)
(slime-setup)

;;
;; Yasnippet
;;

;; Load before autocomplete

(require 'yasnippet)
;; (setq yas-snippet-dirs
;;       (if (null yas-snippet-dirs)
;;           '()
;;         yas-snippet-dirs))
;; (add-to-list 'yas-snippet-dirs *snippet-dir*)
;; (yas-global-mode 1)

;;
;; zlc - Zsh completion
;;

(require 'zlc)
(zlc-mode t)

;; (define-key minibuffer-local-map (kbd "<down>")  'zlc-select-next-vertical)
;; (define-key minibuffer-local-map (kbd "<up>")    'zlc-select-previous-vertical)
;; (define-key minibuffer-local-map (kbd "<right>") 'zlc-select-next)
;; (define-key minibuffer-local-map (kbd "<left>")  'zlc-select-previous)
;; (define-key minibuffer-local-map (kbd "C-c") 'zlc-reset)

;;
;; Ruby mode
;;

;;
;; Load before autocomplete
;;
;; http://cx4a.org/software/rsense/manual.html
;;

(require 'ruby-mode)

;; RSense
(setq rsense-home (getenv "$RSENSE_HOME"))
($add-load-path (concat rsense-home "/etc"))
(require 'rsense)

;; With Pry
;; https://github.com/Mon-Ouie/ruby-dev.el
(require 'ruby-dev)
(autoload 'turn-on-ruby-dev "ruby-dev" nil t)

(add-hook 'ruby-mode-hook 'turn-on-ruby-dev)
($auto-load-mode '("\\Rakefile$" "\\.mab$") 'ruby-mode)

;;
;; Autocomplete
;;

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-sources
      '(ac-source-filename
        ac-source-functions
        ;; ac-source-yasnippet
        ac-source-variables
        ac-source-symbols
        ac-source-features
        ac-source-abbrev
        ac-source-words-in-same-mode-buffers
        ac-source-dictionary))

(auto-complete-mode 1)
(setq ac-fuzzy-enable t)

(add-hook 'ruby-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-rsense-method)
            (add-to-list 'ac-sources 'ac-source-rsense-constant)))

;;
;; Scheme & Racket
;;
;; Quack doc: http://www.neilvandyke.org/quack/quack.el
;; Geiser doc: http://www.nongnu.org/geiser

(require 'geiser)

;; Quack should be loaded after geiser
(require 'quack)

;;
;; jedi for Python auto-completion
;;

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;;
;; Auto pairing brackets
;;

(require 'autopair)
(autopair-global-mode)

(require 'paredit)

(defadvice paredit-mode (around disable-autopairs-around (arg))
  "Disable autopairs mode if paredit-mode is turned on."
  ad-do-it
  (if (null ad-return-value)
      (autopair-mode 1)
    (autopair-mode 0)))

(ad-activate 'paredit-mode)

(add-hook 'emacs-lisp-mode-hook       '$load-paredit-mode)
(add-hook 'lisp-mode-hook             '$load-paredit-mode)
(add-hook 'lisp-interaction-mode-hook '$load-paredit-mode)
(add-hook 'scheme-mode-hook           '$load-paredit-mode)
(add-hook 'geiser-repl-mode-hook      '$load-paredit-mode)

;;; Use with ElDoc
(require 'eldoc)
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

;;; Use with SLIME REPL
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;;; Use with eletric RET
;; If RETURN is pressed when the cursor is before a closing paren, the
;; following code will add an extra newline. The extra newlines are
;; re-gathered by paredit-close-round, which ParEdit binds to “)” by
;; default

;;; cmpitg's
(defvar electrify-return-match
  "[\]\)]"
  "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

;; (defvar electrify-return-match
;;   "[\]}\)\"]"
;;   "If this regexp matches the text after the cursor, do an \"electric\"
;;   return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match'
  then open and indent an empty line between the cursor and the
  text.  Move the cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

;; Using local-set-key in a mode-hook is a better idea.
(global-set-key (kbd "RET") 'electrify-return-if-match)

;;
;; Speedbar in the same frame
;;

;; (require 'sr-speedbar)

;;
;; MultiScratch
;;

;; http://www.emacswiki.org/emacs/MultiScratch

(load-file "~/emacs-config/emacs-local-packages/multi-scratch/multi-scratch.el")
(require 'multi-scratch)

;; 
;; ibus-mode
;; 

(load-file "~/emacs-config/emacs-local-packages/ibus.el/ibus-dev.el")

(require 'ibus)

;; Use C-SPC for Set Mark command
(ibus-define-common-key ?\C-\s nil)
;; Use C-/ for Undo command
(ibus-define-common-key ?\C-/ nil)
;; Change cursor color depending on IBus status
(setq ibus-cursor-color '("red" "blue" "limegreen"))
(setq ibus-agent-file-name "~/emacs-config/emacs-local-packages/ibus.el/ibus-el-agent")

;;
;; Add thing-at-point function
;;

(require 'thingatpt)

;;
;; Support for multiple cursors
;;

;; https://github.com/emacsmirror/multiple-cursors

(require 'multiple-cursors)

;;
;; Markdown-mode
;; 

(require 'markdown-mode)
(require 'markdown-mode+)
($auto-load-mode '("\\.md$" "\\.markdown$") 'markdown-mode)

;;
;; Haskell mode
;;

(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;;
;; PicoLisp mode
;;

($add-load-path "~/emacs-config/emacs-local-packages/picolisp-mode")

(require 'picolisp)
(setq picolisp-program-name "~/opt/picolisp/bin/plmod")
($auto-load-mode "\\.l$" 'picolisp-mode)
(add-hook 'picolisp-mode-hook
          (lambda ()
            (paredit-mode +1)
            (tsm-mode)
            (define-key picolisp-mode-map (kbd "RET") 'newline-and-indent)
            (define-key picolisp-mode-map (kbd "C-h") 'paredit-backward-delete)))

;;
;; Enable to ability to show trailing whitespace
;;

(require 'whitespace)

;;
;; Helm for completion framework
;;

(require 'helm-config)

(eval-after-load "helm-regexp"
  '(helm-attrset 'follow 1 helm-source-moccur))

;; Don't auto change-dir
(setq-default helm-ff-auto-update-initial-value nil)

;;
;; Smex for enhancing M-x
;;

(require 'smex)
(smex-initialize)

;;
;; Saving hooks
;;

;; Make shebang-ed files executable

(add-hook 'after-save-hook '$make-executable)

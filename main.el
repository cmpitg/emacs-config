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
;;; * Discover Helm menu
;;; * Discover construct regexp menu and make screencast
;;; * Discover and write about `auto-insert`
;;; * helm-locate-library
;;; * DONE Rename this file
;;; * DONE Global variables are placed in a seperated file
;;; * Load everything with `eval-after-load` so config would be messed up if a
;;;   package is not loaded
;;; * Better interface to ELPA and el-get:
;;;   - Check if a package exists
;;;   - Check for package dependencies
;;;   - Update package DB
;;;   - Automatically check for updates
;;;   - Update packages
;;;   - Async download and installation with progress bar (later-do)
;;; * DONE [task] Decouple: defining & implementing (global vars at one place)
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

;;
;; Load all the convenient functions
;;

(load-file *custom-functions-path*)

;;
;; Start Emacs server
;;

($server-start)

;; Install packages

(apply '$install-packages ($list-packages-to-be-loaded))
;; (apply 'el-get-install *el-get-package-list*)

;; Add all the load path

(mapc '(lambda (dir)
         (add-to-list 'load-path (format "%s%s" *elpa-package-dir* dir)))
      (directory-files *elpa-package-dir* nil ".*"))

;;
;; cmpitg's specific config
;;

($load-custom-el "keymap-common.el")

;;
;; Load all but disabled packages (defined in `package-list.el`)
;;

(dolist (package-symbol ($list-packages-to-be-loaded))
  (require package-symbol))

;;
;; Now config all default packages
;;

;;
;; File management with dired
;;
;; Default: -lahF


(eval-after-load 'dired-details+ 
  '(setq dired-listing-switches "-lhFgG --group-directories-first"))

;;
;; Powerful and beautiful modeline
;;
;; Repo (bad): https://github.com/milkypostman/powerline
;; Another (better): https://github.com/jonathanchu/emacs-powerline

(eval-after-load 'powerline)

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

(eval-after-load 'popwin)
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
;; (eval-after-load 'tabbar-rul)
)

;;
;; Smartscan helps jumping between occurrences of a symbol
;;

(eval-after-load 'smartscan)
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

(eval-after-load 'rainbow-delimiters)
(global-rainbow-delimiters-mode)
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;
;; Sunrise commander
;;

(eval-after-load 'sunrise-commander)
(eval-after-load 'sunrise-x-loop)
(eval-after-load 'sunrise-x-tabs)
(eval-after-load 'sunrise-x-tree)
(eval-after-load 'sunrise-x-modeline)

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

(eval-after-load 'magit)

;;
;; Monky - Magit for Mercurial
;;

(eval-after-load 'monky)

;;
;; Acme-like mouse chord
;;
;; https://github.com/akrito/acme-mouse.git

(load "~/emacs-config/emacs-local-packages/acme-mouse/acme-mouse.el")

;;
;; Open with file with external application
;;

(eval-after-load 'openwith)
(openwith-mode t)

;;
;; dash - modern Emacs Lisp APIs
;;

;; https://github.com/magnars/dash.el

(eval-after-load 'dash)
(eval-after-load "dash" '(dash-enable-font-lock))

;;
;; JSON mode
;;

(eval-after-load 'json)

;;
;; Expand region - marking based-on semantic
;;

(eval-after-load 'expand-region)

;;
;; Redo mode
;;

;; (eval-after-load 'redo+ nil 'error)

;;
;; Gist
;;

;; https://github.com/defunkt/gist.el

(eval-after-load 'gist)

;;
;; Smooth scrolling
;;

(eval-after-load 'smooth-scrolling)

;;
;; JavaScript
;;

;; (eval-after-load 'js3-mode)
;; (setq js3-auto-indent-p t
;;       js3-enter-indents-newline t
;;       js3-indent-on-enter-key t)

(eval-after-load 'js2-mode)
(add-hook 'js-mode-hook 'js2-minor-mode)
($auto-load-mode '("\\.js\\'") 'js2-mode)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(add-hook 'html-mode-hook '$auto-reload-firefox-after-save-hook)
(add-hook 'css-mode-hook '$auto-reload-firefox-after-save-hook)

;;
;; Firefox integration setup
;;

($add-load-path "~/emacs-config/emacs-local-packages/moz/")
(eval-after-load 'moz)
(add-hook 'javascript-mode-hook '$setup-moz-javascript)
(add-hook 'js3-mode-hook '$setup-moz-javascript)

;;
;; Predictive abbreviation
;;

;; (eval-after-load 'pabbrev)
;; (global-pabbrev-mode)

;;
;; Slime for Common Lisp development
;;

(eval-after-load 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(setq inferior-lisp-program *default-lisp-repl-path*)
(slime-setup)

;;
;; Yasnippet
;;

;; Load before autocomplete

(eval-after-load 'yasnippet)

(eval-after-load 'yasnippet
  '(add-to-list 'yas-snippet-dirs (expand-file-name *snippet-dir*)))

(yas-global-mode 1)

;;
;; zlc - Zsh completion
;;

(eval-after-load 'zlc)
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

(eval-after-load 'ruby-mode)

;; RSense
(setq rsense-home (getenv "$RSENSE_HOME"))
($add-load-path (concat rsense-home "/etc"))
(eval-after-load 'rsense)

;; With Pry
;; https://github.com/Mon-Ouie/ruby-dev.el
(eval-after-load 'ruby-dev)
(autoload 'turn-on-ruby-dev "ruby-dev" nil t)

(add-hook 'ruby-mode-hook 'turn-on-ruby-dev)
($auto-load-mode '("\\Rakefile$" "\\.mab$") 'ruby-mode)

;;
;; Autocomplete
;;

(eval-after-load 'auto-complete)
(eval-after-load 'auto-complete-config)
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

(eval-after-load 'geiser)

;; Quack should be loaded after geiser
(eval-after-load 'quack)

;;
;; jedi for Python auto-completion
;;

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;;
;; Auto pairing brackets
;;

(eval-after-load 'autopair)
(autopair-global-mode)

(eval-after-load 'paredit)

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
(eval-after-load 'eldoc)
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

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

;; (eval-after-load 'sr-speedbar)

;;
;; MultiScratch
;;

;; http://www.emacswiki.org/emacs/MultiScratch

(load-file "~/emacs-config/emacs-local-packages/multi-scratch/multi-scratch.el")
(eval-after-load 'multi-scratch)

;; 
;; ibus-mode
;; 

(load-file "~/emacs-config/emacs-local-packages/ibus.el/ibus-dev.el")

(eval-after-load 'ibus
  '(progn
     ;; Use C-SPC for Set Mark command
     (ibus-define-common-key ?\C-\s nil)
     ;; Use C-/ for Undo command
     (ibus-define-common-key ?\C-/ nil)
     ;; Change cursor color depending on IBus status
     (setq ibus-cursor-color '("red" "blue" "limegreen"))
     (setq ibus-agent-file-name "~/emacs-config/emacs-local-packages/ibus.el/ibus-el-agent")))

;;
;; Add thing-at-point function
;;

(eval-after-load 'thingatpt)

;;
;; Support for multiple cursors
;;

;; https://github.com/emacsmirror/multiple-cursors

(eval-after-load 'multiple-cursors)

;;
;; Markdown-mode
;; 

(eval-after-load 'markdown-mode)
(eval-after-load 'markdown-mode+)
($auto-load-mode '("\\.md$" "\\.markdown$") 'markdown-mode)

;;
;; Haskell mode
;;

(eval-after-load 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;;
;; PicoLisp mode
;;

($add-load-path "~/emacs-config/emacs-local-packages/picolisp-mode")

(eval-after-load 'picolisp)
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

(eval-after-load 'whitespace)

;;
;; Helm for completion framework
;;

(eval-after-load 'helm-config)

(eval-after-load "helm-regexp"
  '(helm-attrset 'follow 1 helm-source-moccur))

;; Don't auto change-dir
(setq-default helm-ff-auto-update-initial-value nil)

;;
;; Smex for enhancing M-x
;;

(eval-after-load 'smex)
(smex-initialize)

;;
;; Saving hooks
;;

;; Make shebang-ed files executable

(add-hook 'after-save-hook '$make-executable)

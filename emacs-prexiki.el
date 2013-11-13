;; -*- mode: emacs-lisp -*-

;;;
;;; TODO
;;; * write documentation/make screencast about workflow
;;; * make "~/emacs-config" a constant    
;;; * YAML to config generate Emacs Lisp config file :-)
;;;

;; Global constants

(setenv "$RSENSE_HOME" (expand-file-name "~/emacs-config/rsense"))

(setq *custom-functions-path*  "~/emacs-config/emacs-cmpitg-config/custom-functions.el"
      *snippet-dir*            "~/emacs-config/snippets"
      *elpa-package-dir*       "~/.emacs.d/elpa/"
      *custom-els-dir*         "~/emacs-config/emacs-cmpitg-config/"
      *default-lisp-repl-path* (expand-file-name "~/bin/sbcl")
      *package-list*     '(redo+
                           color-theme
                           rainbow-delimiters
                           ;; powerline
                           smooth-scrolling
                           auto-complete
                           jedi
                           whitespace
                           ruby-mode
                           yaml-mode
                           haskell-mode
                           markdown-mode
                           markdown-mode+
                           sr-speedbar
                           paredit
                           autopair
                           auto-complete
                           helm
                           slime
                           smex
                           evil
                           evil-nerd-commenter
                           evil-leader
                           evil-paredit
                           thingatpt
                           multiple-cursors
                           dash
                           expand-region
                           yasnippet
                           gist
                           json
                           quack
                           geiser
                           js2-mode
                           pabbrev
                           ))

;; cmpitg's specific configuration

(load-file *custom-functions-path*)

($load-custom-el "keymap-common.el"
                 "aliases.el"
                 )

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
;; Setting the environment
;;

(setq search-highlight 1)

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
;; Highlight brackets
;;

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;
;; Acme-like mouse chord
;;
;; https://github.com/akrito/acme-mouse.git

(load "~/emacs-config/emacs-local-packages/acme-mouse/acme-mouse.el")

;;
;; dash - modern Emacs Lisp APIs
;;

;; https://github.com/magnars/dash.el

(require 'dash)

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

(require 'pabbrev)
(global-pabbrev-mode)

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

(require 'sr-speedbar)

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
            (define-key picolisp-mode-map (kbd "C-h") 'paredit-backward-delete) ) )

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

;;
;; All user-defined customization goes here
;;

(if ($file-exists? "~/emacs-custom.el")
    (load "~/emacs-custom.el"))

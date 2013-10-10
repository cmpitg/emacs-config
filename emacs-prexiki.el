;; -*- mode: emacs-lisp -*-

;;;
;;; TODO: write documentation/make screencast about workflow
;;;

;; Global constants

(setq *elpa-package-dir* "~/.emacs.d/elpa/"
      *custom-els-dir*   "~/emacs-config/emacs-cmpitg-config/"
      *package-list*     '(redo+
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
                           ;; autopair
                           smartparens
                           auto-complete
                           helm
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
                           js3-mode
                           pabbrev
                           ))

;; cmpitg's specific configuration

(load-file "~/emacs-config/emacs-cmpitg-config/custom-functions.el")

($load-custom-el "keymap-ergo.el"
                 "keymap-common.el"
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
(set-fill-column 78)

;; Disable backup file
(setq make-backup-files nil)

;;; Auto complete switching buffer mode
(iswitchb-mode t)

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

(require 'js3-mode)
(setq js3-auto-indent-p t
      js3-enter-indents-newline t
      js3-indent-on-enter-key t)

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

;;
;; Yasnippet
;;

(require 'yasnippet)
(yas-global-mode 1)

;;
;; Autocomplete
;;

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-sources
      '(ac-source-filename
        ac-source-functions
        ac-source-yasnippet
        ac-source-variables
        ac-source-symbols
        ac-source-features
        ac-source-abbrev
        ac-source-words-in-same-mode-buffers
        ac-source-dictionary))
(auto-complete-mode 1)
(setq ac-fuzzy-enable t)

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

;; (require 'autopair)
;; (add-hook 'find-file-hook (lambda () (autopair-mode 1)))
;; (add-hook 'lisp-mode-hook (lambda () (autopair-mode nil)))

(require 'smartparens)
(require 'smartparens-config)

(require 'paredit)

(add-hook 'emacs-lisp-mode-hook '$load-paredit-mode)
(add-hook 'lisp-mode-hook '$load-paredit-mode)
(add-hook 'lisp-interaction-mode-hook '$load-paredit-mode)

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
;; Ruby mode
;;

(require 'ruby-mode)
($auto-load-mode '("\\Rakefile$") 'ruby-mode)

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

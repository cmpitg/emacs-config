(defvar *package-list* '(redo+
                         popwin
                         color-theme
                         rainbow-delimiters
                         smooth-scrolling
                         smartscan
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
                         openwith
                         evil
                         evil-nerd-commenter
                         evil-leader
                         evil-paredit
                         thingatpt
                         multiple-cursors
                         dash
                         ;; tabbar-ruler
                         expand-region
                         yasnippet
                         gist
                         json
                         quack
                         geiser
                         js2-mode
                         pabbrev
                         dired-details+
                         sunrise-commander
                         sunrise-x-tabs
                         sunrise-x-loop
                         sunrise-x-checkpoints
                         sunrise-x-tree
                         sunrise-x-modeline
                         fic-mode
                         magit
                         monky
                         zlc
                         hexrgb)
  "List of packages using in this Emacs configuration.")

(defvar *el-get-package-list* '(;; one-key
                                later-do ; Async function call with timer
                                )
  "List of packages not available in ELPA but available to install with el-get.")

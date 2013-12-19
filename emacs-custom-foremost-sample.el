;;; This is a sample foremost configuration

;; Some essential variables, you can leave it as-is.  Default values are
;; defined in `config-default/custom-functions`.

(setq *ctags-path* "/usr/bin/ctags"
      *snippet-dir* "~/emacs-config/snippets"
      *license-dir* "~/emacs-config/license-list"
      *me* "John Doe <john@example.com>")

;; Disable all Sunrise Commander-related packages.  For list of pre-loaded
;; packages, see `package-list.el`

(setq *disabled-package-list*
      '(sunrise-commander
        sunrise-x-tabs
        sunrise-x-loop
        sunrise-x-checkpoints
        sunrise-x-tree
        sunrise-x-modeline))

;; Load more packages besides default ones

(setq *user-package-list*
      '(tabbar-ruler))

;;; This is a sample foremost configuration

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

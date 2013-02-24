;;; This file should be load after custom functions are all loaded

;; <menu> key is really convenient, so first we get rid of its default
;; use

(global-set-key (kbd "<menu>") 'nil)

;;
;; ErgoEmacs bindings
;;

($load-custom-el "keymap-ergo.el")

;;
;; Bookmark jumping
;;

(global-set-key (kbd "C-h DEL") '(lambda ()
                                   "Jump to keymap config"
                                   (interactive)
                                   ($open-file "~/.emacs_cmpitg/keymap-common.el")))

;;
;; Buffer
;;

(global-set-key (kbd "<f2>") 'save-buffer)
(global-set-key (kbd "C-<f2>") 'buffer-menu)
(global-set-key (kbd "<f3>") '$open-file-gui)
(global-set-key (kbd "<S-f3>") '$open-file-gui-other-window)
(global-set-key (kbd "C-<f9>") '$move-to-compilation-buffer)
(global-set-key (kbd "C-<f4>") '(lambda () (interactive)
                                 (kill-buffer (current-buffer))))
(global-set-key (kbd "M-<f4>") 'recentf-open-files)
(global-set-key (kbd "C-<f1>") '$switch-to-scratch)
(global-set-key (kbd "<menu> <menu>") 'other-window)

(global-set-key (kbd "C-M-v") '(lambda ()
                                (interactive)
                                (scroll-other-window 6)))
(global-set-key (kbd "C-M-S-v") '(lambda ()
                                (interactive)
                                (scroll-other-window -6)))

(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "s-B") '(lambda ()
                               "Switch to last buffer"
                               (interactive)
                               (switch-to-buffer (other-buffer))))
(global-set-key (kbd "<f8>") 'sr-speedbar-toggle)

;;
;; Text
;;

(global-set-key (kbd "C-<f3>") 'grep)
(global-set-key (kbd "C-<f5>") 'previous-error)
(global-set-key (kbd "C-<f7>") 'next-error)
(global-set-key (kbd "C-o") '$open-line)
(global-set-key (kbd "C-S-O") '$open-line-before)
(global-set-key (kbd "C-=") 'align-regexp)

(global-set-key (kbd "<menu> C-<return>") 'complete-symbol)
(global-set-key (kbd "<C-return>") 'complete-tag)

(global-set-key (kbd "s-w") 'whitespace-cleanup)

(global-set-key (kbd "<M-f7>") '(lambda () "Next DEBUG" (interactive) (search-forward "DEBUG")))
(global-set-key (kbd "<S-M-f7>") '(lambda () "Previous DEBUG" (interactive) (search-backward "DEBUG")))
(global-set-key (kbd "<M-f5>") '(lambda () "Next FIXME" (interactive) (search-forward "FIXME")))
(global-set-key (kbd "<S-M-f5>") '(lambda () "Previous FIXME" (interactive) (search-backward "FIXME")))

;;
;; Executing
;;

(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "C-<f12>") '$open-shell)
(global-set-key (kbd "s-a") '$exec-in-other-window)
(global-set-key (kbd "s-A") '$exec-then-pipe)
(global-set-key (kbd "M-s-a") '$pipe-then-exec)
(global-set-key (kbd "M-s-A") '$pipe-then-exec-in-other-window)
;; (global-set-key [mouse-3] '(lambda ()
;;                              "Execute or eval"
;;                              (interactive)
;;                              ($eval-or-exec-print ($current-selection))))
(global-set-key (kbd "<S-down-mouse-1>") nil)
(global-set-key (kbd "<S-mouse-1>") '$exec-then-pipe-selection)

(global-set-key (kbd "<s-menu> <s-menu>") '$exec-in-other-window)
(global-set-key (kbd "<s-menu> s-!") '$exec-then-pipe)
(global-set-key (kbd "<s-menu> s-@") '$pipe-then-exec)
(global-set-key (kbd "<s-menu> s-\\") '$pipe-then-exec-in-other-window)

;;
;; Window
;;

(global-set-key (kbd "S-<f4>") 'delete-window)
(global-set-key (kbd "<f4>") 'find-file-other-window)
(global-set-key (kbd "C-7") 'split-window-vertically)
(global-set-key (kbd "C-5") 'split-window-horizontally)
(global-set-key (kbd "C-%") 'delete-other-windows)

(global-set-key [C-mouse-2] 'split-window-vertically)
(global-set-key [M-mouse-2] 'split-window-horizontally)
(global-set-key [S-mouse-2] 'delete-window)

;;
;; Misc
;;

(global-set-key (kbd "C-/") '$toggle-comment-region)
(global-set-key (kbd "C-M-_") 'redo)
(global-set-key (kbd "<f1>") '$man-this)
(global-set-key (kbd "<mouse-2>") '$eval-selection)

(define-key emacs-lisp-mode-map (kbd "<f1>") '(lambda ()
                                               (interactive)
                                               (apropos (current-word))))
(define-key emacs-lisp-mode-map (kbd "M-<f1>") 'apropos)
(global-set-key (kbd "<menu> M-t t") '(lambda ()
                                       (interactive)
                                       (set-frame-parameter nil
                                        'alpha 78)))

;;
;; Mode
;;

(global-set-key (kbd "C-<menu> C-f") 'auto-fill-mode)
(global-set-key (kbd "C-<menu> C-p") 'paredit-mode)
(global-set-key (kbd "C-<menu> C-e") 'evil-mode)
(global-set-key (kbd "C-<menu> C-w") 'whitespace-mode)
(global-set-key (kbd "C-M-S-SPC") '$toggle-ibus)
(global-set-key (kbd "<C-menu> C-a") 'auto-complete-mode)
(global-set-key (kbd "s-z") '$open-current-file-as-admin)
(global-set-key (kbd "s-v") 'package-list-packages)
(global-set-key (kbd "s-\\") 'ibus-mode)
;; (global-set-key (kbd "M-x") 'execute-extended-command)
;; (global-set-key (kbd "M-/") 'dabbrev-expand)

;; 
;; Mode specific
;; 


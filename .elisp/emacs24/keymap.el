(global-set-key (kbd "<menu>") 'nil)

(global-set-key (kbd "<f2>") 'save-buffer)
(global-set-key (kbd "<C-f2>") 'bs-show)
(global-set-key (kbd "<f3>") 'find-file)
(global-set-key (kbd "S-<f3>") 'find-file)
(global-set-key (kbd "C-x C-f") 'ifind)
(global-set-key (kbd "C-<f3>") 'grep)
(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "C-<f9>") '$move-to-compilation-buffer)
(global-set-key (kbd "C-<f4>") 'kill-this-buffer)
(global-set-key (kbd "S-<f4>") 'delete-window)
(global-set-key (kbd "<f4>") 'find-file-other-window)
(global-set-key (kbd "s-z") '$open-current-file-as-admin)

;; ;; (global-set-key (kbd "<f5>") 'tim-previous-buffer)
;; (global-set-key (kbd "C-<f5>") 'previous-error)
;; ;; (global-set-key (kbd "<f7>") 'tim-next-buffer)
(global-set-key (kbd "C-<f7>") 'next-error)
(global-set-key (kbd "C-<f12>") '(lambda ()
                                   "Open shell"
                                   (interactive)
                                   (split-window-vertically)
                                   (other-window 1)
                                   (shell)))

(global-set-key (kbd "s-b") 'bookmark-jump)
(global-set-key (kbd "<C-f1>") '(lambda ()
                                  (interactive)
                                  (switch-to-buffer "*scratch*")))

;; ;; (global-set-key (kbd "C-M-_") 'redo)
;; (global-set-key (kbd "<f1>") (lambda ()
;;                                (interactive)
;;                                (manual-entry (current-word))))
;; (define-key emacs-lisp-mode-map (kbd "<f1>") '(lambda ()
;;                                                (interactive)
;;                                                (apropos (current-word))))
;; (define-key emacs-lisp-mode-map (kbd "M-<f1>") 'apropos)

;;; quick toggle
(global-set-key (kbd "C-<menu> C-f") 'auto-fill-mode)

;;; emacs lisp
(global-set-key (kbd "s-\\ s-p") 'eval-print-last-sexp)

(global-set-key (kbd "C-M-v") '(lambda ()
                                (interactive)
                                (scroll-other-window 6)))
(global-set-key (kbd "C-M-S-v") '(lambda ()
                                (interactive)
                                (scroll-other-window -6)))

;; Disabile in place of helm
;; (global-set-key (kbd "s-m") 'bs-show)
(global-set-key (kbd "s-v") 'list-packages)
(global-set-key (kbd "s-B") '(lambda ()
                               "Switch to last buffer"
                               (interactive)
                               (switch-to-buffer (other-buffer))))
(global-set-key (kbd "s-@") '$duplicate-line)

(global-set-key (kbd "<menu> <menu>") 'other-window)
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; (global-set-key (kbd "C-M-S-SPC") 'tim-ibus-toggle)

;; ;; execute shell command, get rid of ugly M-!, C-u M-!, C-u M-|

;; execute in a different buffer
(global-set-key (kbd "s-a") '$exec-in-other-window)
(global-set-key (kbd "s-A") '$exec-then-pipe)
(global-set-key (kbd "M-s-a") '$pipe-then-exec)
(global-set-key (kbd "M-s-A") '$pipe-then-exec-in-other-window)

(global-set-key (kbd "C-7") 'split-window-vertically)
(global-set-key (kbd "C-5") 'split-window-horizontally)
(global-set-key (kbd "C-%") 'delete-other-windows)
(global-set-key (kbd "C-o") '$open-line)

(global-set-key (kbd "s-$") '$clipboard<-pwd)

(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-S-O") '$open-line-before)

;; (global-set-key (kbd "s-K") '$delete-line)

;; (global-set-key (kbd "<C-menu> C-h") 'hs-minor-mode)

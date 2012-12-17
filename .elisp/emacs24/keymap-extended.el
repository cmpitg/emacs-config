;; -*- mode: emacs-lisp -*-

;; Functions

;; Quick mode toggling
(global-set-key (kbd "C-<menu> C-(") 'autopair-mode)
(global-set-key (kbd "C-<menu> C-e") 'evil-mode)
(global-set-key (kbd "C-<menu> C-a") 'auto-complete-mode)

;; Redo
(global-set-key (kbd "C-M-_") 'redo)

;; Enhanced buffer list
(global-set-key (kbd "s-m") 'helm-mini)

;; Enhanced M-x
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-X") 'smex)
(global-set-key (kbd "<f3>") 'helm-find-files)
(global-set-key (kbd "s-b") 'helm-bookmarks)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Toggle ibus-mode
(global-set-key (kbd "C-M-S-SPC") '$ibus-toggle)

;; Buffer switching
(global-set-key (kbd "<C-next>") 'tabbar-forward)
(global-set-key (kbd "<C-prior>") 'tabbar-backward)

;; Yasnippet
(global-set-key (kbd "<C-tab>") 'yas/expand)
(global-set-key (kbd "<menu> y f") '$goto-snippets-folder)
(global-set-key (kbd "<menu> y t") 'yas/tryout-snippet)
(global-set-key (kbd "<menu> y v") 'yas/visit-snippet-file)
(global-set-key (kbd "<menu> y l") 'yas/load-snippet-buffer)
(global-set-key (kbd "<menu> y a") 'tim-add-new-snippet)

;; Keymap visiting
(global-set-key (kbd "<menu> k k") '$goto-keymap-ext-config)
(global-set-key (kbd "<M-f7>") '$goto-next-DEBUG)
(global-set-key (kbd "<S-M-f7>") 'goto-prev-DEBUG)
(global-set-key (kbd "<M-f5>") '$goto-next-FIXME)
(global-set-key (kbd "<S-M-f5>") '$goto-prev-FIXME)

;; Code browsing
;;; ECB
(global-set-key (kbd "<f6>") 'tim-ecb-toggle)
(global-set-key (kbd "<M-f6>") 'ecb-toggle-layout)

;; Ack
(global-set-key (kbd "<C-f3>") 'ack)

;; Completion
(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "M-RET") 'rsense-complete)))

;; Align
(global-set-key (kbd "C-=") 'align-regexp)

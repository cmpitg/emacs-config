;; Load Ergo keymap for Programmer Dvorak keyboard

($load-custom-el "keymap-ergo-programmer-dvorak.el")

;;
;; Bookmark jumping
;;

(global-set-key (kbd "C-h DEL") '(lambda ()
                                   "Jump to keymap config"
                                   (interactive)
                                   ($open-file "~/emacs-config/emacs-cmpitg-config/keymap-common.el")))

(global-set-key (kbd "C-h <return>") '(lambda ()
                                        "Jump to ~/emacs-config/README.md"
                                        (interactive)
                                        ($open-file "~/emacs-config/README.md")))


;; -*- mode: emacs-lisp -*-

(defun -load-files-if-exists- (&rest paths)
  "Load files when they exists."
  (dolist (file-path paths)
   (when (file-exists-p file-path)
     (load file-path))))

;; Pre-customization

(-load-files-if-exists- "~/emacs-custom-foremost.el")

;; Main

(-load-files-if-exists- "~/emacs-config/emacs-cmpitg-config/emacs-environment.el"
                        "~/emacs-config/emacs-prexiki.el")

;; All user-defined customization goes here

(-load-files-if-exists- "~/emacs-custom.el")

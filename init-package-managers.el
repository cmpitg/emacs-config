;;
;; Copyright (C) 2012-2013 Duong H. Nguyen <cmpitgATgmaildotcom>
;;
;; bogoengine is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; bogoengine is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Description:
;;
;; This file initializes the package manager and loads all the essential
;; packages that are required for this (cmpitg) configuration to function
;; properly
;;

(defvar *must-have-package-list*
  '(dash                                ; "Modern" list processing
    ht                                  ; The missing hashtable library
    s                                   ; "Modern" string processing
    cl                                  ; Common Lisp subset in Emacs Lisp
    cl-lib                              ; Common Lisp library
    )
  "List of packages that are vital to this config and must be
installed and loaded.")

;;
;; ELPA
;;

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("SC" . "http://joseito.republika.pl/sunrise-commander/"))

(package-initialize)

;; Fetch the list of available packages
(when (not package-archive-contents)
  (package-refresh-contents))

;;
;; el-get - yet another sophisticated package manager
;;
;; https://github.com/dimitri/el-get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  ;; Install or update
  (cond
   ((fboundp 'el-get-self-update)
    (el-get-self-update))

   (t
    (url-retrieve
     "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
     (lambda (s)
       (goto-char (point-max))
       (eval-print-last-sexp))))))

(add-to-list 'el-get-recipe-path "~/emacs-config/el-get-user/recipes")
(el-get 'sync)

;;
;; Add all local packages list in `$HOME/emacs-config/emacs-local-packages` to
;; `loadpath`
;;

(let ((local-package-dir "~/emacs-config/emacs-local-packages/"))
  (dolist (dirname (directory-files local-package-dir))
    (add-to-list 'load-path
                 (concat local-package-dir dirname))))

;;
;; Install and load all must-have packages with ELPA
;;

(dolist (package *must-have-package-list*)
  (when (not (package-installed-p package))
    (package-install package)
    (require package)))

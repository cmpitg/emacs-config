;;
;; Copyright (C) 2012-2013 Duong H. Nguyen <cmpitgATgmaildotcom>
;;
;; This project is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This project is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

(require 'el-get)

(defun $el-get-package-list ()
  "Get the list of packages cached in el-get repositories.  This
function doesn't update el-get database.  Returns a plist of format

\(:name package-name :description package-description\)"
  (el-get-read-all-recipes))

(defun $el-get-package-exists? (package-symbol)
  "Determine if a package exists in el-get repositories.  This
function doesn't update local el-get database."
  (not (null
        (memq package-symbol (-map (lambda (package)
                                     (plist-get package :name))
                                   ($el-get-package-list))))))

(defun $elpa-package-exists? (package-symbol)
  "Determine if a package exists in ELPA.  This function doesn't
update local ELPA database."
  (not (null (memq package-symbol (-map (lambda (element)
                                          (car element))
                                        ($elpa-get-package-list))))))

(defun $elpa-get-package-list ()
  "Get the list of packages information cached in your ELPA repositories."
  package-archive-contents)

(defun $elpa-get-installed-package-list ()
  "Get the list of packages information installed in your ELPA repositories.

This function return the value of `package-alist` variable. Which
returns an alist of all packages available for activation.

Each element has the form (PKG . DESC), where PKG is a package
name (a symbol) and DESC is a vector that describes the package.

The vector DESC has the form [VERSION-LIST REQS DOCSTRING].
  VERSION-LIST is a version list.
  REQS is a list of packages required by the package, each
   requirement having the form (NAME VL) where NAME is a string
   and VL is a version list.
  DOCSTRING is a brief description of the package."
  package-alist)

(defun $install-packages (&rest packages)
  "Install a list of package if not installed."
  (dolist (package-name packages)
    (unless (or ($package-installed? package-name)
                (memq package-name *local-package-list*))
      (cond
       (($el-get-package-exists? package-name)
        (el-get-install package-name))
       (($elpa-package-exists? package-name)
        (package-install package-name))))))

(defun $package-installed? (package-symbol)
  "Determine if a package is installed."
  (or (package-installed-p package-symbol)
      (el-get-package-is-installed package-symbol)
      ($local-package-is-installed? package-symbol)))

(defun $local-package-is-installed? (package-symbol)
  "Determine if a package is installed at
`$HOME/emacs-config/emacs-local-packages`."
  ;; TODO to be implemented
  nil)

(defun $list-packages-to-be-loaded ()
  "List all packages to be loaded when Emacs is initialized,
i.e. all packages in `*elpa-package-list*' and
`*user-package-list*' but NOT in `*user-disable-package-list*'."
  (-filter (lambda (package)
             (not (member package *user-disable-package-list*)))
           (-concat *user-package-list*
                    *local-package-list*
                    *elpa-package-list*
                    *el-get-package-list*)))

(defun $install-or-update-el-get ()
  "Install/update el-get."
  (interactive)
  (cond
   (($is-function-defined? 'el-get-self-update)
    (el-get-self-update))

   (t
    (url-retrieve
     "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
     (lambda (s)
       (goto-char (point-max))
       (eval-print-last-sexp))))))


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

(setenv "$RSENSE_HOME" (expand-file-name "~/emacs-config/rsense"))

;; (defvar *openwith-associations*
;;   '(("\\.pdf\\'" "evince" (file))
;;     ("\\.mp3\\'" "smplayer" (file))
;;     ("\\.odt\\'" "libreoffice" (file))
;;     ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mp4\\|m4v\\)\\'" "smplayer" (file))
;;     ("\\.\\(?:jp?g\\|png\\)\\'" "eog" (file)))
;;   "Determine which program is used to open which filetype with
;;  `openwith' library.")

(setq openwith-associations
  '(("\\.pdf\\'" "evince" (file))
    ("\\.mp3\\'" "smplayer" (file))
    ("\\.odt\\'" "libreoffice" (file))
    ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mp4\\|m4v\\)\\'" "smplayer" (file))
    ("\\.\\(?:jp?g\\|png\\)\\'" "eog" (file))))

(setq *custom-functions-path*  "~/emacs-config/config-default/custom-functions.el"
      *snippet-dir*            "~/emacs-config/snippets"
      *custom-els-dir*         "~/emacs-config/config-default/"
      *default-lisp-repl-path* (expand-file-name "~/.bin/sbcl")
      *elpa-package-dir*       "~/.emacs.d/elpa/")

;;; Ignore this variable if you don't use Xiki
(defvar *xiki-path*
  "/usr/share/emacs/site-lisp"
  "Path to Xiki gem's site-lisp directory.")

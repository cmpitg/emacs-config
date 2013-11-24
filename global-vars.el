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

(setenv "$RSENSE_HOME" (expand-file-name "~/emacs-config/rsense"))

(setq openwith-associations
      '(("\\.pdf\\'" "evince" (file))
        ("\\.mp3\\'" "smplayer" (file))
        ("\\.odt\\'" "libreoffice" (file))
        ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mp4\\|m4v\\)\\'" "smplayer" (file))
        ("\\.\\(?:jp?g\\|png\\)\\'" "eog" (file))))

(setq *custom-functions-path*  "~/emacs-config/emacs-cmpitg-config/custom-functions.el"
      *snippet-dir*            "~/emacs-config/snippets"
      *custom-els-dir*         "~/emacs-config/emacs-cmpitg-config/"
      *default-lisp-repl-path* (expand-file-name "~/bin/sbcl")
      *elpa-package-dir*       "~/.emacs.d/elpa/")

;;; Ignore this variable if you don't use Xiki
(defvar *xiki-path*
  "/home/cmpitg/.rvm/gems/ruby-1.9.3-p448/gems/trogdoro-el4r-1.0.9/data/emacs/site-lisp/"
  "Path to Xiki gem's site-lisp directory.")

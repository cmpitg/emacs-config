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

(defun $insert-into-emacs-lisp-docstring (string)
  "Interactive command.  Prompt and insert a string and escape it
as Emacs Lisp docstring format.

E.g.

\($insert-into-emacs-lisp-docstring \"\(message \\\"hola mundo!\\\"\)\"\)
"
  (interactive "MInput your string: ")
  (insert (s-replace-all '(("\\" . "\\\\\\\\")
                           ("(" . "\\\\(")
                           (")" . "\\\\)")
                           ("\"" . "\\\\\""))
                         string)))

(defun $add-bracket-and-eval (&optional string)
  "TODO"
  (interactive)
  (let* ((preprocessed-sexp (cond ((not ($string-empty? string))
                                   string)
                                  (($is-selecting?)
                                   ($get-selection))
                                  (t
                                   (read-string "Command: "))))
         (sexp (if (not (and (s-starts-with? "(" preprocessed-sexp)
                             (s-ends-with?   ")" preprocessed-sexp)))
                 (format "(%s)" preprocessed-sexp)
                 preprocessed-sexp)))
    ($eval-string sexp)))

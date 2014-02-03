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

(defun $popup-shell-command (&optional command)
  "Run a non-interactive shell command and popup a window to
display result."
  (interactive)
  (let* ((command-str (cond ((not ($string-empty? command))
                             command)
                            ((is-selecting?)
                             ($get-selection))
                            (t
                             ($read-string "Shell command: "))))
         (output ($exec command-str)))
    ($popup-message output)))

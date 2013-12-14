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

;;
;; Eshell
;;

(defun $eshell-history ()
  "Display eshell commands as with M-x.  The selected command is
added to the current eshell buffer."
  (interactive)
  (insert
   (ido-completing-read "Eshell history: "
                        (delete-dups
                         (ring-elements eshell-history-ring)))))

(defun $switch-to-eshell-back-and-forth ()
  "Switch to eshell if current is not eshell, and switch to last
active buffer if current buffer is eshell."
  (interactive)
  (cond ((string-match-p "\\*.*eshell.*\\*" ($current-buffer-name))
         ($switch-to-last-buffer))
        (t
         (eshell))))

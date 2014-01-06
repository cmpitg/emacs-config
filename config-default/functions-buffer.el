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

(defun $next-buffer ()
  "Move to the next non-special buffer, unless it's *scratch*."
  (interactive)
  (let* ((name "") (pos nil) (stop nil))
    (while (null stop)
      (setf name (buffer-name (next-buffer)))
      (setf pos (string-match "*" name))
      (if (string= "*scratch*" name) (setf stop t))
      (if (or (null pos)
              (> pos 0)) (setf stop t)))))

(defun $move-to-compilation-buffer ()
  "Move to *compilation* buffer if it exists."
  (interactive)
  (if (find "*compilation*" (mapcar #'buffer-name (buffer-list))
            :test #'equal)
    (switch-to-buffer "*compilation*")))

(defun $previous-buffer ()
  "Move to the previous non-special buffer, unless it's *scratch*."
  (interactive)
  (let* ((name "") (pos nil) (stop nil))
    (while (null stop)
      (setf name (buffer-name (previous-buffer)))
      (setf pos (string-match "*" name))
      (if (string= "*scratch*" name) (setf stop t))
      (if (or (null pos)
              (> pos 0)) (setf stop t)))))

(defun $current-buffer-name ()
  "Retrieve the name of the current buffer."
  (buffer-name (current-buffer)))

(defun $kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun $switch-to-last-buffer ()
  "Switch to last buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun $insert-text-at-the-end ()
  "Insert current selected text at the end of current buffer."
  (interactive)
  (call-interactively 'kill-ring-save)
  (end-of-buffer)
  (call-interactively 'yank))

(defun $last-sexp ()
  "Return the sexp right before the current cursor."
  (interactive)
  (preceding-sexp))

(defun $geiser-repl-process ()
  "Return the process behind current Geiser REPL."
  (let ((repl-buffer (get-buffer "* Racket REPL *")))
    (if repl-buffer
      (get-buffer-process repl-buffer)
      nil)))

(defun $geiser-send-string (string)
  "Evaluate last sexp with Geiser and send it to the REPL."
  (interactive)
  (let ((string-to-send (cond ((not ($string-empty? string))
                               string)
                              (($is-selecting?)
                               ($get-selection))
                              (t
                               ($read-string "String: ")))))
    (comint-send-string ($geiser-repl-process) string)))

(defun $current-line-comment-syntax ()
  "Return the current line-comment syntax for current buffer mode."
  comment-start)

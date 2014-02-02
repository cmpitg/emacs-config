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

(defun $surround (begin-string end-string)
  "Surround current selection with `begin-string` at the
beginning and `end-string` at the end.  If selection is not
active, insert `begin-string` and `end-string` and place the
cursor in-between them."
  (interactive "sStart string: \nsEnd string: ")
  (cond 
   (($is-selecting?)
    (save-excursion
      (let ((start-point ($selection-start))
            (end-point   ($selection-end)))
        ($goto-point start-point)
        (insert begin-string)
        ($goto-point end-point)
        (forward-char (length begin-string))
        (insert end-string))))

   (t
    (insert (concat begin-string end-string))
    (backward-char (length end-string)))))

(defun $markdown-italicize ()
  "Italicize selection or adding italic format."
  (interactive)
  ($surround "*" "*"))

(defun $markdown-embolden ()
  "Embolden selection or adding bold format."
  (interactive)
  ($surround "**" "**"))

(defun $markdown-rawify ()
  "Rawify selection or adding raw format."
  (interactive)
  ($surround "`" "`"))

(defun $move-to-beginning-of-line ()
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line."
  (interactive)

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line nil))))

(defun $duplicate-line ()
  "Duplicate current line."
  (interactive)
  (beginning-of-line)
  (kill-line)
  (yank)
  (newline)
  (yank)
  (beginning-of-line)
  (previous-line))

(defun $open-line (arg)
  "Open line and move to the next line."
  (interactive "p")
  (end-of-line)
  (delete-horizontal-space)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

(defun $open-line-before (arg)
  "Open line and move to the previous line."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

(defun $toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles from 3 cases: UPPER CASE, lower case, Title Case, in that
cyclic order."
  (interactive)
  (let (pos1 pos2 (deactivate-mark nil) (case-fold-search nil))
    (if (and transient-mark-mode mark-active)
      (setq pos1 (region-beginning)
            pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'word))
            pos2 (cdr (bounds-of-thing-at-point 'word))))

    (unless (eq last-command this-command)
      (save-excursion
        (goto-char pos1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]")
          (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]")
          (put this-command 'state "all caps"))
         ((looking-at "[[:upper:]][[:lower:]]")
          (put this-command 'state "init caps"))
         (t (put this-command 'state "all lower"))
         )))
    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region pos1 pos2)
      (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region pos1 pos2)
      (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region pos1 pos2)
      (put this-command 'state "all lower")))))

(defun $fix-hard-wrapped-region (begin end)
  "Fix hard-wrapped paragraphs."
  (interactive "r")
  (shell-command-on-region begin end "fmt -w 2500" nil t))

(defun $mark-word ()
  "Put point at beginning of current word, set mark at end."
  (interactive)
  (let* ((opoint (point))
         (word (current-word))
         (word-length (length word)))
    (if (save-excursion
          ;; Avoid signaling error when moving beyond buffer.
          (if (> (point-min)  (- (point) word-length))
            (beginning-of-buffer)
            (forward-char (- (length word))))
          (search-forward word (+ opoint (length word))
                          'noerror))
      (progn (push-mark (match-end 0) nil t)
             (goto-char (match-beginning 0)))
      (error "No word at point" word))))

(defun $mark-line ()
  "Mark current line."
  (interactive)
  (beginning-of-line)
  (push-mark (point) t t)
  (end-of-line))

(defun $insert-me ()
  "Insert my information."
  (interactive)
  (insert *me*))

(defun $delete-line ()
  "Delete current line."
  (interactive)
  (beginning-of-line)
  (kill-line)
  (kill-line))

(defun $get-text (start end)
  "Return text from current buffer between start and end point."
  (if (or (< start (point-min))
          (< (point-max) end))
    ""
    (buffer-substring start end)))

(defun $current-char ()
  "Return the string representing the character at the current
cursor position."
  ($get-text (point) (+ 1 (point))))

(defun $peek-char ()
  "Peek next character, return the string representing it.."
  ($get-text (+ 1 (point)) (+ 2 (point))))

(defun $join-line ()
  "Join next line with the current line.  This is just a
convenient wrapper of `join-line'."
  (interactive)
  (join-line -1))

(defun $selection-start ()
  "Return the position of the start of the current selection."
  (region-beginning))

(defun $selection-end ()
  "Return the position of the end of the current selection."
  (region-end))

(defun $is-selecting? ()
  "Determine if a selection is being held."
  (region-active-p))

(defun $current-selection ()
  "Return the current selected text."
  (if ($is-selecting?)
    (buffer-substring ($selection-start)
                      ($selection-end))
    ""))

(defun $get-selection ()
  "Return the current selected text."
  ($current-selection))

(defun $delete-selected-text ()
  "Delete the selected text, do nothing if none text is selected."
  (if ($is-selecting?)
    (delete-region ($selection-start) ($selection-end))))

(defalias 'get-selection '$get-selection)

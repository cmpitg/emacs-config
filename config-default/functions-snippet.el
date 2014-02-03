;;
;; Copyright (C) 2014 Duong H. Nguyen <cmpitgATgmaildotcom>
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

(require 'cl)

(defun* create-snippet (&optional mode
                                  &key
                                  filename
                                  mode
                                  abbrev
                                  short-description)
  "Create a snippet for `mode` and open that newly created
snippet.  The snippet is place inside my
`~/emacs-config/snippets/[mode]/[file-name]`.  Other arguments
are self-explanatory.  The snippet directory is created if it
doesn't exist yet."
  (interactive)
  (let* ((mode (if (string-empty? mode)
                 ($read-string "Mode: "
                               :initial-input (format "%s" major-mode))
                 mode))
         (snippet-mode-dir (f-expand (format "~/emacs-config/snippets/%s" mode)))

         (abbrev (if (string-empty? abbrev)
                   (read-string "Abbrev: ")
                   abbrev))

         (snippet-file (if (string-empty? filename)
                         (read-file-name "File name: "
                                         snippet-mode-dir
                                         abbrev)
                         filename))

         (short-description (if (string-empty? short-description)
                              (read-string "Short description: ")
                              short-description)))
    ;; Make sure snippet directory exists
    (unless (f-exists? snippet-mode-dir)
      (f-mkdir snippet-mode-dir))

    (write-to-file snippet-file
                   (s-concat "# -*- mode: snippet -*-\n"
                             "# name: " short-description "\n"
                             "# key: " abbrev             "\n"
                             "# group: " mode             "\n"
                             "# --\n"))
    (find-file snippet-file)))

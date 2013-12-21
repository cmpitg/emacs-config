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

(require 'ht)

(defun ht-to-alist* (table)
  "Deeply convert hashtable to alist.

E.g.

\(ht-to-alist* \(ht \('a \(ht \('b 'c\)\)\)\)\)  ;; => '\(\(a \(b . c\)\)\)
"
  (let ((result (ht-to-alist table)))
    (-map (lambda (key-val)
            (let ((key (car key-val))
                  (val (cdr key-val)))
             (if (hash-table-p val)
               (cons key (ht-to-alist* val))
               (cons key val))))
          result)))

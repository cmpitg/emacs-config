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

(require 'cl)
(require 's)

(defun* $string-start-with? (string substring &key (ignore-case nil))
  "Determine if a string starts with a substring.

E.g.

\($string-start-with? \"config-default\" \"config-\"\)  ;; => t
\($string-start-with? \"config-default\" \"Config-\"\)  ;; => nil"
  (s-starts-with? substring string ignore-case))

(defun* $string-end-with? (string substring &key (ignore-case nil))
  "Determine if a string ends with a substring.

E.g.

\($string-end-with? \"config-default\" \"default\"\)  ;; => t
\($string-end-with? \"config-default\" \"Default\"\)  ;; => nil"
  (s-ends-with? substring string ignore-case))

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

(defun $google (keyword)
  "Google a keyword in Firefox."
  (interactive (list ($read-string "Keyword: "
                                   :initial-input (get-selection))))
  ($open-url-in-firefox
   (format "https://encrypted.google.com/search?q=%s" keyword)))


(defun $open-url-in-firefox (url)
  "Open a URL in Firefox."
  (interactive
   (list ($read-string
          "URL: "
          :initial-input (cond
                          ((is-selecting?)
                           (get-selection))
                          ((thing-at-point-url-at-point)
                           (thing-at-point-url-at-point))
                          (t
                           "https://encrypted.google.com/")))))
  ($send-to-mozrepl (format "switchToTabHavingURI('%s', true)" url)))

(defun $refresh-firefox ()
  "Refresh current tab of Firefox browser."
  (interactive)
  ;; This function can be used when editing HTML/CSS/Web resources, so the
  ;; timeout is there for the file to properly saved.
  ($send-to-mozrepl "setTimeout(BrowserReload, 300)"))

(defun $start-mozrepl ()
  "Start MozRepl."
  (interactive)
  (inferior-moz-start-process))

(defun $send-to-mozrepl (string)
  "Send a string to MozRepl."
  (interactive "MCommand: ")
  ($start-mozrepl)                      ; Make sure MozRepl is up and running
  (comint-send-string (inferior-moz-process)
                      string))


(defun $auto-reload-firefox-after-save-hook ()
  "Auto reload Firefox when saving."
  (add-hook 'after-save-hook
            '(lambda ()
               (interactive)
               (comint-send-string (inferior-moz-process)
                                   "setTimout(BrowserReload(), '1000');"))
            ;; buffer-local
            'append 'local))

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

(global-unset-key (kbd "C-/"))

; Load el4r, which loads Xiki
(add-to-list 'load-path *xiki-path*)
(require 'el4r)
;(el4r-boot)

;;; Xiki environment

;; Let me press "y", instead of type "yes"
(fset 'yes-or-no-p 'y-or-n-p)

;; C-k kills whole line if at beginning
(setq kill-whole-line nil)

;; Make tabe into spaces when you type them
(setq-default indent-tabs-mode nil)
;; Display existing tabs as 2 characters wide
(setq-default tab-width 2)

;; No new frame for ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; Show ediff views side-by-side
(setq ediff-split-window-function 'split-window-horizontally)

;; Set default font
(set-face-attribute 'default nil
  :height 110
  :family "Monaco")

;; Save cursor location upon closing files, and restore upon reopening
(require 'saveplace)
(setq-default save-place t)

;; Don't create #... files when editing
(setq make-backup-files nil)

;; Numbering lines
(require 'linum)
(global-linum-mode 1)

;;; fill-column
(setq-default fill-column 78)
(set-fill-column 78)

;; Change cursor type
;;; 'hbar 'bar or 'box
(set-default 'cursor-type 'bar)

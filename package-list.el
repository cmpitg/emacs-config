;;
;; Copyright (C) 2012-2013 Duong H. Nguyen <cmpitgATgmaildotcom>
;;
;; bogoengine is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; bogoengine is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

(defvar *elpa-package-list* '(popwin
                              color-theme
                              rainbow-delimiters
                              smooth-scrolling
                              smartscan
                              auto-complete
                              jedi
                              whitespace
                              ruby-mode
                              yaml-mode
                              haskell-mode
                              markdown-mode
                              markdown-mode+
                              sr-speedbar
                              paredit
                              autopair
                              auto-complete
                              helm
                              slime
                              smex
                              openwith
                              evil
                              evil-nerd-commenter
                              evil-leader
                              evil-paredit
                              thingatpt
                              multiple-cursors
                              dash
                              expand-region
                              yasnippet
                              gist
                              json
                              quack
                              geiser
                              js2-mode
                              pabbrev
                              dired-details+
                              sunrise-commander
                              sunrise-x-tabs
                              sunrise-x-loop
                              sunrise-x-checkpoints
                              sunrise-x-tree
                              sunrise-x-modeline
                              fic-mode
                              magit
                              monky
                              zlc
                              hexrgb)
  "List of packages using in this Emacs configuration.")

(defvar *el-get-package-list*
  '(later-do                            ; Async eval
    powerline                           ; @johnathanchu version, beautiful
                                        ; modeline
                                        ; https://github.com/jonathanchu/emacs-powerline
    multi-scratch                       ; Multiple scratch buffers
    moz-repl                            ; MozRepl
    )
  "List of packages not available in ELPA but available to install with el-get.")

(defvar *local-package-list*
  '(acme-mouse                          ; Acme-like mouse chords binding
    ibus                                ; iBus interface
    ))

;;; For user customization

(defvar *user-package-list* '()
  "List of packages that user wants to load beside preloaded ones
by default.")

(defvar *user-disable-package-list* '()
  "List of packages in the default set that are not loaded.")

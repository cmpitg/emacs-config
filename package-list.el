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

(defvar *elpa-package-list* 
  '(jedi                                ; Autocomplete and cool stuff for
                                        ; Python development
    rainbow-delimiters                  ; Highlight matching parentheses
    ruby-mode
    ruby-dev                            ; Ruby and Pry
    yaml-mode
    haskell-mode
    markdown-mode
    markdown-mode+
    sr-speedbar                         ; Speedbar in the same frame
    paredit                             ; Parentheses manipulation in Lisp
                                        ; modes
    autopair                            ; Auto-pairing parentheses
    auto-complete
    slime                               ; Best Common Lisp development tool
    openwith                            ; Open file with external program, in
                                        ; any file-browsing modes like Sunrise
                                        ; Commander or Dired
    evil                                ; Vim emulator
    evil-nerd-commenter                 ; Vim commenter
    evil-leader                         ; Vim leader command
    evil-paredit                        ; Paredit-like mode for evil
    yasnippet                           ; Textmate-like snippet and better
    gist                                ; Interface to Github's gist
    json                                ; JSON lib for Emacs
    quack                               ; Another mode for Scheme development
    geiser                              ; Scheme development
    js2-mode                            ; JavaScript
    pabbrev                             ; Better abbrev
    dired-details+                      ; Better Dired information control
    sunrise-commander                   ; File manager
    sunrise-x-tabs
    sunrise-x-loop                      ; Execute commands async in SR
    sunrise-x-checkpoints
    sunrise-x-tree                      ; Tree browsing feature for SR
    sunrise-x-modeline                  ; Nicer modeline in SR
    magit                               ; Best Git interface for Emacs
    monky                               ; Magit-like mode for Mercurial
    zlc                                 ; Zsh completion
    hexrgb                              ; Color manipulation
    )
  "List of packages using in this Emacs configuration.")

(defvar *el-get-package-list*
  '(later-do                            ; Async eval
    powerline                           ; @johnathanchu version, beautiful
                                        ; modeline
                                        ; https://github.com/jonathanchu/emacs-powerline
    multi-scratch                       ; Multiple scratch buffers
    moz-repl                            ; MozRepl
    whitespace                          ; Display trailing whitespace
    )
  "List of packages not available in ELPA but available to install with el-get.")

(defvar *local-package-list*
  '(acme-mouse                          ; Acme-like mouse chords binding
    ibus                                ; iBus interface
    rsense                              ; Comprehensive Ruby development
    picolisp
    ))

;;; For user customization

(defvar *user-package-list* '()
  "List of packages that user wants to load beside preloaded ones
by default.")

(defvar *user-disable-package-list* '()
  "List of packages in the default set that are not loaded.")

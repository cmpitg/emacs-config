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

;; TODO
;;
;; * Document
;;   - List packages in ELPA and el-get
;;   - Determine if a package exists
;;   - Determine if a package is installed
;; * Goto package directory
;; * Remove package
;; * Simple way to add menu
;; * Make all the config files packages
;; * Default compile command for filetype
;; * Hide compilation buffer
;; * Add license (GUI)
;; * Add license header (GUI)
;; * Better semantic for interactive
;; * Adding $smart-backward-expression
;; * Some cases to use with browser:
;;   - When editing HTML/CSS/Javascript:
;;     * Send buffer to MozRepl
;;     * Send region to MozRepl
;;     * Browser auto-reload
;;     * Switch to repl then switch back
;;   - Google a selection with $google
;;   - Refresh the browser with $refresh-firefox
;;   - Send command to MozRepl with $send-to-mozrepl
;;   - Open a URL on Firefox with $open-url-in-firefox
;; * Better helm-grep: remember last search
;;

;;;
;;; Customization
;;;

(defvar *$emacs-lisp-keywords*
  '("$defalias")
  "List of symbols we want to treat as keywords.")

(defvar *$emacs-lisp-functions*
  '("$auto-load-mode")
  "List of symbols we want to treat as \"special\" functions.")

(require 'cl)                           ; Common Lisp subset
(require 'cl-lib)                       ; Some Common Lisp libraries
(require 'dash)                         ; Functional Emacs Lisp
(require 's)                            ; Better string APIs
(require 'f)                            ; Better APIs to work with file and
                                        ; directories
(require 'ht)                           ; Better hashtable APIs

;; Current directory
(setq _custom-function-dir_
      (file-name-directory (or load-file-name buffer-file-name "")))

;;
;; List of convenient functions to be loaded.
;;
;; IMPORTANT NOTE: these files are not modules in a sense that they are not
;; separatedly functional without being loaded with each other.  If you intend
;; to load just one or a few but not all, read the source code carefully.
;; Order of loading matters.
;;

(setq _convenient-functions-path-list_
      '("functions-eshell.el"
        "functions-package-manager.el"
        "functions-text.el"
        "functions-hashtable.el"
        "functions-emacs-lisp-utils.el"
        "functions-string.el"
        "functions-buffer.el"
        "functions-web-browser.el"
        "functions-shell.el"
        "functions-files.el"
        ))

(dolist (path _convenient-functions-path-list_)
  (load (format "%s%s"
                _custom-function-dir_
                path)))

;;;
;;; Functions
;;;

;;; Others

(defun $symbol->string (symbol)
  "Convert a symbol to a string."
  (symbol-name symbol))

(defun $string->symbol (string)
  "Convert a string into an uninterned symbol."
  (make-symbol string))

(defun $list-dir (path)
  "List a directory content."
  (directory-files path))

(defun $read-file (path)
  "Read file and return file content as string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun $join-strings (separator a-seq)
  "Join strings.  Works with any type of sequence and any data type as its element.

E.g.

\($join-strings \"|\" '\(\"a\" \"b\" \"c\"\)\) ; => \"a|b|c\"
\($join-strings \"|\" [1 \"b\" c]\) ; => \"1|b|c\""
  (-reduce (lambda (result element)
             (format "%s%s%s" result separator element))
           (-map (lambda (x) x) a-seq)))

(defun* $popup-message (content &key (buffer-name "*Temporary*"))
  "Display a popup window with CONTENT as its content and an
optional BUFFER-NAME name.  Require popwin extension.  Press ESC
or C-g to close the window.

E.g.

;; Display \"Hello World\" in a popup window.
\($popup-message \"Hello World\"\)

;; Display \"Hola Mundo\" in a popup window, naming that window buffer \"*mundo*\"
\($popup-message \"Hello World\" :buffer-name \"*mundo*\"\)
"
  (require 'popwin)
  (with-output-to-temp-buffer buffer-name
    (princ content)))

(defun* $download-file (url filepath &key (overwrite nil))
  "Download a file.

E.g.

;; Download, raise an error if the file exists
\($download-file \"https://raw.github.com/defunkt/gist.el/master/gist.el\"
		\"/tmp/gist.el\"\)
c
;; Download and overwrite if already exists
\($download-file \"https://raw.github.com/defunkt/gist.el/master/gist.el\"
		\"/tmp/gist.el\"
		:overwrite t\)"
  (interactive "MURL: \nFSave to: ")
  (url-copy-file url filepath overwrite))

(defun $scm-status ()
  "Call for the corresponding SCM `status` command."
  (interactive)
  (let ((current-scm ($get-scm)))
    (cond
     ((string= "git" current-scm)
      (magit-status nil))

     ((string= "hg" current-scm)
      (monky-status))

     (t nil))))

(defun $get-scm ()
  "Return the current source control management (SCM) of current
file as string."
  (interactive)
  (let ((mode-name (downcase
                    (replace-regexp-in-string " \\|[[:digit:]]\\|:.*\\|-.*" "" vc-mode))))
    mode-name))

(defun $sunrise ()
  "Open Sunrise Commander, remove the nonpane buffer."
  (interactive)
  (unless sr-running
    (sunrise)
    (sr-reset-view-remove-nonpane-buffer)))

(defun $sunrise-cd ()
  "Open Sunrise Commander with current directory, remove the
nonpage buffer."
  (interactive)
  (unless sr-running
    (sunrise-cd)
    (sr-reset-view-remove-nonpane-buffer)))

(defun sr-reset-view ()
  "Reset Sunrise Commander pane view."
  (interactive)
  (when sr-running
    (sr-setup-windows)))

(defun sr-reset-view-remove-nonpane-buffer ()
  "Reset Sunrise Commander pane view, removing the nonpane
buffer."
  (interactive)
  (when sr-running
    (sr-setup-windows)
    (windmove-down)
    (delete-window)))

(defun* $read-string (prompt &key
                             (initial-input         nil)
                             (history               nil)
                             (default-value         nil)
                             (inherit-input-method  nil))
  "An alias of read-string, with keyword arguments.  See
`read-string' documentation for more details.

  Read a string from the minibuffer."
  (read-string prompt
               initial-input
               history
               default-value
               inherit-input-method))

(defun $new-home-script (file-name)
  "Creating a new file/script at ~/bin/."
  (interactive (list ($read-string "File name (~/bin/): "
                                   :initial-input "~/bin/")))
  ($open-file file-name))

(defun $custom-els-path (suffix)
  "Return the path of the custom Emacs Lisp configuration.
Default: `~/emacs-config/config-default/`."
  (unless ($is-var-defined? '*custom-els-dir*)
    (setq *custom-els-dir* "~/emacs-config/config-default/"))
  (concat *custom-els-dir* suffix))

(defun $load-custom-el (&rest filenames)
  "Load customization file."
  (dolist (file filenames)
    (let ((file-path ($custom-els-path file)))
      (when ($file-exists? file-path) (load-file file-path)))))

(defun $current-path ()
  "Get full path of the current file."
  (interactive)
  (if buffer-file-name
    buffer-file-name
    ""))

(defun $eval-then-replace-last-exp ()
  "Eval region then replace last expression with result."
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))

(defun $eval-then-replace ()
  "Eval region then replace region with result."
  (interactive)
  (let ((value ($eval-string ($get-selection))))
    (kill-region ($selection-start) ($selection-end))
    (insert (format "%s" value))))

(defun $server-start (&rest dir)
  "Start an Emacs server in a specific socket directory.  If no
directory is specified, the default dir /tmp/emacs1000/ is used."
  (if dir (setq server-socket-dir dir))
  (server-start))

(defun $add-load-path (path)
  "Add path to load-path."
  (add-to-list 'load-path path))

(defun $set-frame-size-according-to-resolution ()
  "Set frame size based on current resolution"
  (interactive)
  (if window-system
    (progn
      ;; Largest displays: 120 (width)
      ;; Smaller ones:     100 (width)
      ;; Smallest ones:     80 (width)
      (if (> (x-display-pixel-width) 1280)
	    (add-to-list 'default-frame-alist (cons 'width 200))
	    (if (> (x-display-pixel-width) 1024)
          (add-to-list 'default-frame-alist (cons 'width 150))
          (add-to-list 'default-frame-alist (cons 'width 80))))
      (add-to-list 'default-frame-alist
                   (cons 'height (/ (- (x-display-pixel-height) 100)
                                    (frame-char-height)))))))

(defun $modify-opacity (&optional dec)
  "Modify the opacity of emacs frame; if DEC is t,
increase the opacity."
  (let* ((alpha-or-nil (frame-parameter nil 'alpha))
         (old-alpha (if alpha-or-nil alpha-or-nil 100))
         (new-alpha (if dec (- old-alpha 10) (+ old-alpha 10))))
    (when (and (>= new-alpha frame-alpha-lower-limit) (<= new-alpha 100))
      (modify-frame-parameters nil (list (cons 'alpha new-alpha))))))

(setq *is-ibus-on?* nil)
(defun $toggle-ibus ()
  "Toggle ibus."
  (interactive)
  (if (null *is-ibus-on?*)
    (progn (ibus-enable)
           (setf *is-ibus-on?* t))
    (progn (ibus-disable)
           (setf *is-ibus-on?* nil))))

(setq *is-ecb-running?* nil)
(defun $toggle-ecb ()
  "Toggle ECB."
  (interactive)
  (if (null *is-ecb-running?*)
    (progn (ecb-activate)
           (setf *is-ecb-running?* t))
    (progn (ecb-deactivate)
           (setf *is-ecb-running?* nil))))

(defun $helm-multi-all ()
  "multi-occur in all buffers backed by files."
  (interactive)
  (helm-multi-occur
   (delq nil
         (mapcar (lambda (b)
                   (when (buffer-file-name b) (buffer-name b)))
                 (buffer-list)))))

(defun $setup-moz-javascript ()
  "Setting JavaScript mode with MozRepl."
  (moz-minor-mode 1))

(defun $load-paredit-mode ()
  "Load paredit mode and disable autopair."
  (paredit-mode t)
  (when ($is-function-defined? 'autopair-mode)
    (autopair-mode 0))
  (when ($is-function-defined? 'smartparens-mode)
    (smartparens-mode 0)))

(defun $put-mode-line-to-top ()
  "Put the mode-line to the top of the window."
  (setq header-line-format mode-line-format mode-line-format nil))

(defun $is-var-defined? (symbol)
  "Check if the variable corresponding to the symbol is defined.

E.g.

\($is-var-defined? 'a-random-symbol-unlikely-to-be-defined\)  ; => nil
\($is-var-defined? 'c-mode-map\)                              ; => t"
  (boundp symbol))

(defun $is-function-defined? (symbol)
  "Check if the function corresponding to the symbol is defined.

E.g.
\($is-function-defined? 'a-random-symbol-unlikely-to-be-defined\)  ; => nil
\($is-function-defined? '$is-function-defined?\)                   ; => t"
  (fboundp symbol))

(defun $delete-current-file ()
  "Delete the file associated with the current buffer.
Delete the current buffer too."
  (interactive)
  (let (currentFile)
    (setq currentFile (buffer-file-name))
    (when (yes-or-no-p (concat "Delete file: " currentFile))
      (kill-buffer (current-buffer))
      (delete-file currentFile)
      (message (concat "Deleted file: " currentFile)) ) ) )

(defun $open-current-file-as-admin ()
  "Open the current buffer as *nix root.
This command works on `sudo` *nixes only."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

(defun $create-tags (dir-name)
  "Create tags file using exuberant ctags."
  (interactive "DDirectory: ")
  (shell-command
   ;; (format "%s -f %s/TAGS -e -R %s *.c *.h *.cpp *.hpp *.java *.php *.js *.pas" *ctags-path*
   ;;         dir-name (directory-file-name dir-name))
   (format "%s %s -e" *ctags-path* dir-name)))

(defun $save-macro (name)
  "Take a name as argument and save the last defined macro."
  (interactive "SName of the macro: ")
  (kmacro-name-last-macro name)                ; Use this name for the macro
  (find-file "~/emacs-config/saved-macros.el") ; Load the macro file
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)               ; Copy the macro
  (newline)
  (save-buffer)
  (kill-buffer)
  ;; (switch-to-buffer nil)
  )

(defun $make-executable ()
  "chmod +x current file."
  (interactive)
  (and
   (save-excursion
     (save-restriction
       (widen)
       (goto-char (point-min))
       (save-match-data
         (looking-at "^#!"))))
   (not (file-executable-p buffer-file-name))
   (shell-command (concat "chmod u+x " (shell-quote-argument buffer-file-name)))
   (revert-buffer)
   (message
    (concat "Saved as script: " buffer-file-name))))

(defun $compile-haml ()
  "Compile HAML file to HTML file."
  (interactive)
  (and (string-match ".*\.haml$" ($current-file-full-path))
       (let ((output-file (replace-regexp-in-string "\.haml$" ".html"
                                                    ($current-file-full-path))))
         (compile (concat "haml \"" ($current-file-full-path) "\" "
                          "\"" output-file "\"")))))

(defun $compile-coffee ()
  "Compile CoffeeScript to JavaScript."
  (interactive)
  (and (string-match ".*\.coffee$" ($current-file-full-path))
       (compile (concat "coffee -c " ($current-file-full-path)))))

(defun $compile-livescript ()
  "Compile LiveScript to JavaScript."
  (interactive)
  (and (string-match ".*\.ls$" ($current-file-full-path))
       (compile (concat "livescript -c -d " ($current-file-full-path)))))

(defun $unicode-symbol (name)
  "Translate a symbolic name for a Unicode character -- e.g.,
 LEFT-ARROW or GREATER-THAN into an actual Unicode character
 code."
  (decode-char 'ucs (case name
                      (left-arrow 8592)
                      (up-arrow 8593)
                      (right-arrow 8594)
                      (down-arrow 8595)
                      (double-vertical-bar #X2551)
                      (equal #X003d)
                      (not-equal #X2260)
                      (identical #X2261)
                      (not-identical #X2262)
                      (less-than #X003c)
                      (greater-than #X003e)
                      (less-than-or-equal-to #X2264)
                      (greater-than-or-equal-to #X2265)
                      (logical-and #X2227)
                      (logical-or #X2228)
                      (logical-neg #X00AC)
                      ('nil #X2205)
                      (horizontal-ellipsis #X2026)
                      (double-exclamation #X203C)
                      (prime #X2032)
                      (double-prime #X2033)
                      (for-all #X2200)
                      (there-exists #X2203)
                      (element-of #X2208)
                      (square-root #X221A)
                      (squared #X00B2)
                      (cubed #X00B3)
                      (lambda #X03BB)
                      (alpha #X03B1)
                      (beta #X03B2)
                      (gamma #X03B3)
                      (delta #X03B4))))

(defun $substitute-pattern-with-unicode (pattern symbol)
  "Add a font lock hook to replace the matched part of PATTERN
with the Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
  (font-lock-add-keywords
      nil `((,pattern
             (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                       ,(unicode-symbol symbol)
                                       'decompose-region)
                       nil))))))

(defun $substitute-patterns-with-unicode (patterns)
  "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
  (mapcar #'(lambda (x)
              (substitute-pattern-with-unicode (car x)
                                               (cdr x)))
          patterns))

(defun $add-new-snippet (mode name)
  "Add new yassnipet snippet."
  (interactive "MMode (without the `-mode` part): \nMSnippet name: ")
  (let* ((mode (format "%s-mode" mode))
         (yas-file (format "%s/%s/%s" *snippet-dir* mode name)))
    (message "file: %s" yas-file)
    (find-file yas-file)))

(defun $add-license (license-file)
  "Add license."
  (interactive "sLicense (CaSE-SeNSitIVE): ")
  (insert-file (format "%s/%s.txt" *license-dir* license-file)))

(defun $clipboard<-region (begin end)
  "Copy region to clipboard."
  (clipboard-kill-ring-save begin end))

(defun $kill-ring<- (str)
  "Copy a string to the kill ring."
  (interactive "MString: ")
  (kill-new str))

(defun $clipboard<- (str)
  "Copy a string to clipboard."
  (interactive "MString: ")
  (let ((x-select-enable-clipboard t))
    (x-select-text str)))

(defun $clipboard<-pwd ()
  "Copy current directory to clipboard."
  (interactive)
  ($clipboard<- ($current-dir)))

(defun $noweb-code-chunk-add-mode (mode-name)
  "Add mode to a code chunk"
  (interactive "MMode name (without `-mode`): ")
  (insert (concat "-*- mode: " mode-name " -*-")))

(defun $autoload-mode (file-regex mode-symbol)
  "Add autoload mode when opening file.
Example: ($autoload-mode \"Rakefile\" . 'ruby-mode)"
  (add-to-list 'auto-mode-alist (cons file-regex mode-symbol)))

(defun $exec (command)
  "Execute a shell command then return its value as string."
  (interactive "MCommand: ")
  (shell-command-to-string command))

(defun $exec-in-other-window (command)
  "Execute in other window."
  (interactive "MCommand: ")
  (shell-command command))

(defun $exec-then-pipe (command)
  "Execute and pipe output to the current buffer."
  (interactive "MCommand: ")
  (shell-command command t))

(defun $exec-then-pipe-selection ()
  "Execute selection and pipe output to the current buffer."
  (interactive)
  ($exec-then-pipe ($current-selection)))

(defun $pipe-then-exec (command)
  "pipe current region to a command, exec it, and pipe the output back."
  (interactive "MCommand: ")
  (shell-command-on-region
   (if mark-active (region-beginning) 1)
   (if mark-active (region-end) 1)
   command t))

(defun $keyboard-quit ()
  "Escape the minibuffer or cancel region consistently using 'Control-g'.
Normally if the minibuffer is active but we lost focus (say, we
clicked away or set the cursor into another buffer) we can quit
by pressing 'ESC' three times. This function handles it more
conveniently, as it checks for the condition of not beign in the
minibuffer but having it active. Otherwise simply doing the ESC
or (keyboard-escape-quit) would brake whatever split of windows
we might have in the frame."
  (interactive)
  (if (not(window-minibuffer-p (selected-window)))
    (if (or mark-active (active-minibuffer-window))
	  (keyboard-escape-quit))
    (keyboard-quit)))

(defun $toggle-evil-local ()
  "Toggle evil-mode for current buffer."
  (interactive)
  (if evil-local-mode
    (progn
      (evil-local-mode -1)
      (setq cursor-type 'bar))
    (evil-local-mode)))

(defun $auto-load-mode (filetypes mode)
  "Autoload mode for filetype regex or a list of filetypes.
Example:
    ($auto-load-mode \"\\\\.rake$\" 'ruby-mode)
    ($auto-load-mode '(\"\\\\.md$\" \"\\\\.markdown$\") 'markdown-mode)"

  (if (stringp filetypes)
    (add-to-list 'auto-mode-alist (cons filetypes mode))
    (dolist (filetype filetypes)
      (add-to-list 'auto-mode-alist (cons filetype mode)))))

(defun $goto-str (str)
  "Go to the next appearance of a string."
  (interactive "MString: ")
  (search-forward str nil t))

(defun $->string (exp)
  "Convert an expression to string."
  (format "%s" exp))

;; (defun $smart-forward-exp ()
;;   "Smart forward expression.  E.g.

;;   |say-hello-to me, 'Jane'
;;   say-hello-to| me, 'Jane'
;;   say-hello-to |me, 'Jane'
;;   say-hello-to me,| 'Jane'
;;   say-hello-to me, |'Jane'
;;   say-hello-to me, 'Jane'|"
;;   (interactive)
;;   (if (and (not (string-equal " " ($peek-char)))
;;            (not (string-equal " " ($current-char))))
;;       (if (null ;; ($goto-str " ")
;;            (search-forward-regexp "[ .]" nil t)
;;                 )
;;           (end-of-buffer)
;;         (backward-char))

;;     (progn (search-forward-regexp "[^[:space:]]" nil t)
;;            (backward-char))))

(defun $goto-snippets-dir ()
  "Go to personal snippets directory."
  (interactive)
  (find-file *snippet-dir*))

(defun $goto-next-DEBUG ()
  "Go to next DEBUG."
  (interactive)
  (search-forward "DEBUG"))

(defun $goto-prev-DEBUG ()
  "Go to prev DEBUG."
  (interactive)
  (search-backward "DEBUG"))

(defun $goto-next-FIXME ()
  "Go to next FIXME."
  (interactive)
  (search-forward "FIXME"))

(defun $goto-prev-FIXME ()
  "Go to prev FIXME."
  (interactive)
  (search-backward "FIXME"))

(defun $open-shell ()
  "Open shell."
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (shell))

(defun $man-this ()
  "`man` this word."
  (interactive)
  (manual-entry (current-word)))

(defun $switch-to-scratch ()
  "Switch to the *scratch* buffer."
  (interactive)
  (unless (string-equal "*scratch*" ($current-buffer-name))
    (switch-to-buffer-other-window "*scratch*")))

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

(defun $eval-string (str)
  "Eval a string."
  (interactive)
  ($eval (read str)))

(defun $string-empty? (str)
  "Determine if a string is empty."
  (= 0 (length str)))

(defun $string-but-last (str)
  "Return a string with its last character removed."
  (if ($string-empty? str) ""
      (substring str 0 -1)))

(defun $string-contains? (str substring)
  "Check if a string contains a substring."
  (not (null (string-match substring str))))

(defun $current-dir ()
  "Current directory."
  (or (file-name-directory (or load-file-name buffer-file-name ""))
      "~"))

(defun $build-open-file-cmd-string ()
  "Build a string used to execute an open-file dialog."
  (concat "zenity --file-selection --filename "
          ($current-dir)
          " 2>/dev/null"))

(defun $fiplr-find-file-other-window ()
  "Open file with fuzzy matching (using fiplr) in other window.
A new window is created if there's current only one window."
  (message-box "To be implemented!"))

(defun $open-file-gui ()
  "Open a file using Zenity."
  (interactive)
  (let ((filename ($string-but-last ($exec ($build-open-file-cmd-string)))))
    (unless ($string-empty? filename)
      ($open-file filename))))

(defun $open-file-gui-other-window ()
  "Open a file using Zenity."
  (interactive)
  (let ((filename ($string-but-last ($exec ($build-open-file-cmd-string)))))
    (unless ($string-empty? filename)
      ($open-file-other-window filename))))

(defun $string-empty? (str)
  "Determine if a string is empty."
  (zerop (length str)))

(defun $first-char-as-string (str)
  "Return the first character of a string as string."
  (if (not ($string-empty? str))
    (substring str 0 1)
    ""))

(defun $last-char-as-string (str)
  "Return the last character of a string as string."
  (if (not ($string-empty? str))
    (let ((len (length str)))
      (substring str (- len 1) len))
    ""))

(defun $mark-word-backward (times)
  "Mark word backward."
  (interactive "p")
  (if ($is-selecting?)
    (kill-region ($selection-start) ($selection-end))
    (progn (if (and (not (eq last-command this-command))
                    (not (eq last-command 'mark-sexp)))
             (set-mark (point)))
           (backward-word times))))

(defun $trim-spaces (text)
  "Trim spaces at the beginning and the end of a portion of text."
  (while (and (not ($string-empty? text))
              (string= " " ($first-char-as-string text)))
    (setf text (substring text 1)))

  (while (and (not ($string-empty? text))
              (string= " " ($last-char-as-string text)))
    (setf text (substring text 0 (- (length text) 1))))

  text)

(defun $is-external-command? (text)
  "Determine if a portion of text indicates an external
command (started with a `!`)."
  (and (not ($string-empty? text))
       (string= "!" ($first-char-as-string text))))

(defun $is-directory? (text)
  "Determine if a portion of text is a directory on the
filesystem."
  (read ($exec (format "[ -d '%s' ] && echo -n t || echo -n nil" text))))

(defun $eval-or-exec-print (command-text)
  "Execute external command then pipe result to the current
buffer or eval an Emacs Lisp expression."
  (let ((command ($trim-spaces command-text)))
    (unless ($string-empty? command)
      (progn
        ($delete-selected-text)
        (insert command)
        (insert "\n")
        (cond (($is-external-command? command)
               ($exec-then-pipe (substring command 1)))

              (($is-directory? command)
               ($exec-in-other-window (format "ls '%s'" command)))

              (t ($eval-string command)))))))

(defun $jekyll-add-last-updated ()
  "Add last_update timestamp with `date -R` format."
  (interactive)
  ($goto-point (point-min))
  (if (re-search-forward "^last_updated:.*$")
    (replace-match (format "last_updated: %s"
                           ($string-but-last ($exec "date -R"))))))

;; (global-set-key (kbd "C-<home>") 'jekyll-add-last-updated)

(defun $evil-define-key (key func)
  "Define keymap in all evil states."
  (define-key evil-normal-state-map key func)
  (define-key evil-insert-state-map key func)
  (define-key evil-visual-state-map key func)
  (define-key evil-replace-state-map key func)
  (define-key evil-operator-state-map key func)
  (define-key evil-motion-state-map key func))

(defun $evil-undefine-helper ()
  "(Helper) Prevent evil from disabling a default Emacs kepmap."
  (interactive)
  (let (evil-mode-map-alist)
    (call-interactively (key-binding (this-command-keys)))))

(defun $evil-undefine (key)
  "(Helper) Prevent evil from disabling a default Emacs kepmap."
  ($evil-define-key key 'evil-undefine))

(defun $defalias (new-symbol old-symbol doc-string)
  "Define an alias NEW-SYMBOL from OLD-SYMBOL with documentation
string.  E.g. ($defalias '$add-snippet '$add-new-snippet \"Add
new snippet\")"
  (defalias new-symbol old-symbol)
  (put new-symbol 'function-documentation doc-string))

($defalias '$smart-forward-exp 'forward-word
  "Forward word")

($defalias '$filter-command '$pipe-then-exec
  "Filter a command")

($defalias '$pipe-then-exec-in-other-window 'shell-command-on-region
  "Filter a command but pipe the other to other window")

($defalias '$current-point 'point
  "Return current position of the keyboard cursor in the
buffer.")

($defalias '$current-word 'current-word
  "Return the current word as string.")

($defalias '$goto-point 'goto-char
  "Goto `point` in the buffer")

($defalias '$add-me '$insert-me
  "Insert me")

($defalias '$eval 'eval
  "Eval an expression")

($defalias '$mark-defun 'mark-defun
  "mark-defun")

($defalias '$eval-selection 'eval-region
  "Evaluate the current selected text")

($defalias '$open-file 'find-file
  "Open a file")

($defalias '$open-file-other-window 'find-file-other-window
  "Open a file in other window")

($defalias '$save-file 'save-buffer
  "Save current buffer")

($defalias '$file-exists? 'file-exists-p
  "Determine if a file exists")

;;;
;;; Settings
;;;

(put 'font-lock-add-keywords 'lisp-indent-function 1)
(put '$defalias 'lisp-indent-function 2)

;;; Better `if' and `list' indentation
(put 'list 'lisp-indent-function nil)
(put 'if 'lisp-indent-function 1)
(put 'quote lisp-indent-function 1)

(font-lock-add-keywords 'emacs-lisp-mode
  (list (cons (eval-when-compile
		(regexp-opt *$emacs-lisp-keywords* 'words))
	      font-lock-keyword-face)
	(cons (eval-when-compile
		(regexp-opt *$emacs-lisp-functions* 'words))
	      font-lock-function-name-face)))

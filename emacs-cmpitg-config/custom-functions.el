;; TODO
;;
;; * Goto package directory
;; * Remove package
;; * Simple way to add menu
;; * Bookmark -> add to menu:
;;   - ~/emacs-config
;;   - ~/emacs-config/init.el
;;   - ~/emacs-config/emacs-cmpitg-config/keymap-common.el
;; * Make all the config files packages
;; * Default compile command for filetype
;; * Hide compilation buffer
;; * Add license (GUI)
;; * Add license header (GUI)
;; * Better semantic for interactive
;;

;;;
;;; Customization
;;;

;; Exuberant ctags tags generating
(setq *ctags-path* "/usr/bin/ctags"
      *snippet-dir* "~/emacs-config/snippets"
      *license-dir* "~/emacs-config/license-list"
      *me* "Duong H. Nguyen <cmpitg AT gmailDOTcom>"
      )

;;;
;;; Definitions
;;;

(defvar *$doc-strings* (make-hash-table)
  "Storing documentation strings.")

(defvar *$emacs-lisp-keywords*   '("$defalias"))
(defvar *$emacs-lisp-functions*  '("$auto-load-mode"))

;;;
;;; Functions
;;;

(defun $custom-els-path (suffix)
  "Return the path of the custom Emacs Lisp configuration."
  (unless ($is-var-defined? '*custom-els-dir*)
    (setq *custom-els-dir* "~/emacs-cmpitg/emacs-cmpitg-config/"))
  (concat *custom-els-dir* suffix))

(defun $load-custom-el (&rest filenames)
  "Load customization file."
  (dolist (file filenames)
    (load-file ($custom-els-path file))))

(defun $install-packages (&rest packages)
  "Install a list of package if not installed."
  (dolist (package-name packages)
    (unless ($package-installed? package-name)
      (package-install package-name))))

(defun $current-path ()
  "Get full path of the current file."
  (interactive)
  (if buffer-file-name buffer-file-name
    ""))

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

(defun $server-start (&rest dir)
  "Start an Emacs server in a specific socket directory.  If no
directory is specified, the default dir /tmp/emacs1000/ is used."
  (if dir (setq server-socket-dir dir))
  (server-start))

(defun $add-load-path (path)
  "Add path to load-path."
  (add-to-list 'load-path path))

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

(defun $put-mode-line-to-top ()
  "Put the mode-line to the top of the window."
  (setq header-line-format mode-line-format mode-line-format nil))

(defun $open-line (arg)
  "Open line and move to the next line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1))

(defun $open-line-before (arg)
  "Open line and move to the previous line."
  (interactive "p")
  (beginning-of-line)
  (open-line arg))

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

(defun $is-var-defined? (symbol)
  "Check if the variable corresponding to the symbol is defined."
  (boundp symbol))

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
  (kmacro-name-last-macro name)                      ; Use this name for the macro
  (find-file "~/.elisp/custom-els/custom-macros.el") ; Load the macro file
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)                            ; Copy the macro
  (newline)
  (save-buffer)
  (kill-buffer)
                                        ;  (switch-to-buffer nil)
  )

(defun $make-executable ()
  "Chmod +x current file."
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
 code. "
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

(defun $autoload-mode (file-regex mode-symbol)
  "Add autoload mode when opening file.
Example: ($autoload-mode \"Rakefile\" . 'ruby-mode)"
  (add-to-list 'auto-mode-alist (cons file-regex mode-symbol)))

(defun $delete-line ()
  "Delete current line."
  (interactive)
  (beginning-of-line)
  (kill-line)
  (kill-line))

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
  (switch-to-buffer "*scratch*"))

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

(defun $current-dir ()
  "Current directory."
  (or (file-name-directory (or load-file-name buffer-file-name ""))
      "~"))

(defun $build-open-file-cmd-string ()
  "Build a string used to execute an open-file dialog."
  (concat "zenity --file-selection --filename "
          ($current-dir)
          " 2>/dev/null"))

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
  "Return the first character of a string as strin.g"
  (if (not ($string-empty? str))
      (substring str 0 1)
    ""))

(defun $last-char-as-string (str)
  "Return the last character of a string as string."
  (if (not ($string-empty? str))
      (let ((len (length str)))
        (substring str (- len 1) len))
    ""))

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
  (puthash new-symbol doc-string *$doc-strings*))

($defalias '$filter-command '$pipe-then-exec
  "Filter a command")

($defalias '$pipe-then-exec-in-other-window 'shell-command-on-region
  "Filter a command but pipe the other to other window")

($defalias '$current-point 'point "")

($defalias '$current-word 'current-word "")

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

($defalias '$package-installed? 'package-installed-p
  "Determine if a package is installed")

($defalias '$file-exists? 'file-exists-p
  "Determine if a file exists")

;;;
;;; Settings
;;;

(put 'font-lock-add-keywords 'lisp-indent-function 1)
(put '$defalias 'lisp-indent-function 2)
(put 'list 'lisp-indent-function nil)

(font-lock-add-keywords 'emacs-lisp-mode
  (list (cons (eval-when-compile
                (regexp-opt *$emacs-lisp-keywords* 'words))
              font-lock-keyword-face)
        (cons (eval-when-compile
                (regexp-opt *$emacs-lisp-functions* 'words))
              font-lock-function-name-face)))

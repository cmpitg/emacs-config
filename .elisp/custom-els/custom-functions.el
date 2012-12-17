;;;
;;; Definitions
;;;

(defvar *$doc-strings* (make-hash-table)
  "Storing documentation strings.")

(defvar *tim-emacs-lisp-keywords*   '("$defalias"))
(defvar *tim-emacs-lisp-functions*  '("$autoload-mode"))

;;;
;;; Functions
;;;

(setq *custom-els* "~/.elisp/custom-els/")

(defun $load-custom-el (&rest filenames)
  "Load customization file"
  (dolist (file filenames)
    (load (format "%s%s" *custom-els*
                  file))))

(defun $current-file-full-path ()
  "Get full path of the current file"
  (interactive)
  (if buffer-file-name buffer-file-name
    ""))

(defun $duplicate-line ()
  "Duplicate current line"
  (interactive)
  (beginning-of-line)
  (kill-line)
  (yank)
  (newline)
  (yank)
  (beginning-of-line)
  (previous-line))

(defun $add-load-path (path)
  "Add path to load-path"
  (add-to-list 'load-path path))

(defun $next-buffer ()
  "Move to the next non-special buffer, unless it's *scratch*"
  (interactive)
  (let* ((name "") (pos nil) (stop nil))
    (while (null stop)
      (setf name (buffer-name (next-buffer)))
      (setf pos (string-match "*" name))
      (if (string= "*scratch*" name) (setf stop t))
      (if (or (null pos)
              (> pos 0)) (setf stop t)))))

(defun $move-to-compilation-buffer ()
  "Move to *compilation* buffer, if it doesn't exist yet, stand
still."
  (interactive)
  (if (find "*compilation*" (mapcar #'buffer-name (buffer-list))
            :test #'equal)
      (switch-to-buffer "*compilation*")))

(defun $previous-buffer ()
  "Move to the previous non-special buffer, unless it's *scratch*"
  (interactive)
  (let* ((name "") (pos nil) (stop nil))
    (while (null stop)
      (setf name (buffer-name (previous-buffer)))
      (setf pos (string-match "*" name))
      (if (string= "*scratch*" name) (setf stop t))
      (if (or (null pos)
              (> pos 0)) (setf stop t)))))

;; Function to change default window height and width
(defun set-frame-size-according-to-resolution ()
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

;; Function to change opacity of emacs window
(defun $opacity-modify (&optional dec)
  "Modify the opacity of emacs frame; if DEC is t,
increase the opacity."
  (let* ((alpha-or-nil (frame-parameter nil 'alpha))
         (old-alpha (if alpha-or-nil alpha-or-nil 100))
         (new-alpha (if dec (- old-alpha 10) (+ old-alpha 10))))
    (when (and (>= new-alpha frame-alpha-lower-limit) (<= new-alpha 100))
      (modify-frame-parameters nil (list (cons 'alpha new-alpha))))))

;; Function to toggle ibus
(setq tim-ibus-is-on-p nil)
(defun tim-ibus-toggle ()
  "Toggle ibus."
  (interactive)
  (if (null tim-ibus-is-on-p)
      (progn (ibus-enable)
             (setf tim-ibus-is-on-p t))
      (progn (ibus-disable)
             (setf tim-ibus-is-on-p nil))))

(defalias '$ibus-toggle 'tim-ibus-toggle)

;; Function to toggle ibus
(setq tim-ecb-running-p nil)
(defun tim-ecb-toggle ()
  "Toggle ECB."
  (interactive)
  (if (null tim-ecb-running-p)
      (progn (ecb-activate)
             (setf tim-ecb-running-p t))
      (progn (ecb-deactivate)
             (setf tim-ecb-running-p nil))))

;; Get the custom elisp file path
(defun tim-custom-els-path (suffix)
  "Return my personal custom elisp path."
  (concat "~/.elisp/custom-els/" suffix))

;; Put the mode-line to the top of the window
(defun $put-mode-line-to-top ()
  (setq header-line-format mode-line-format mode-line-format nil))

;; Vi-like open line
(defun tim-open-line (arg)
  "Open line and move to the next line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1))

(defalias '$open-line 'tim-open-line)

;; Vi-like open line before
(defun tim-open-line-before (arg)
  "Open line and move to the previous line."
  (interactive "p")
  (beginning-of-line)
  (open-line arg))

(defalias '$open-line-before 'tim-open-line-before)

;; Toggle case fix
(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles from 3 cases: UPPER CASE, lower case, Title Case,
in that cyclic order."
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

;; Fixed hard-wrapped paragraphs
(defun fix-hard-wrapped-region (begin end)
  (interactive "r")
  (shell-command-on-region begin end "fmt -w 2500" nil t))

(defun tim-load (file-name)
  "Helper to load my Emacs Lisp files."
  (load-file (tim-custom-els-path file-name)))

;;; Delete current file
(defun delete-current-file ()
  "Delete the file associated with the current buffer.
Delete the current buffer too."
  (interactive)
  (let (currentFile)
    (setq currentFile (buffer-file-name))
    (when (yes-or-no-p (concat "Delete file: " currentFile))
      (kill-buffer (current-buffer))
      (delete-file currentFile)
      (message (concat "Deleted file: " currentFile)) ) ) )

                                        ; from newsgroup gnu.emacs.help, by Richard Riley, 2009-08-02
(defun open-current-file-as-admin ()
  "Open the current buffer as unix root.
This command works on unixes only."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

;; Exuberant ctags tags generating
;; (setq path-to-ctags "/usr/bin/ctags")
(setq path-to-ctags "/home/cmpitg/bin/create-ctags")

(defun create-tags (dir-name)
  "Create tags file using exuberant ctags."
  (interactive "DDirectory: ")
  (shell-command
   ;; (format "%s -f %s/TAGS -e -R %s *.c *.h *.cpp *.hpp *.java *.php *.js *.pas" path-to-ctags
   ;;         dir-name (directory-file-name dir-name))
   (format "%s %s -e" path-to-ctags dir-name)))

(defun save-macro (name)
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
  "Chmod +x a file"
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
  "Compile haml file to html file"
  (interactive)
  (and (string-match ".*\.haml$" ($current-file-full-path))
       (let ((output-file (replace-regexp-in-string "\.haml$" ".html"
                                                    ($current-file-full-path))))
         (compile (concat "haml \"" ($current-file-full-path) "\" "
                          "\"" output-file "\"")))))

(defun $compile-coffee ()
  "Compile CoffeeScript to JavaScript"
  (interactive)
  (and (string-match ".*\.coffee$" ($current-file-full-path))
       (compile (concat "coffee -c " ($current-file-full-path)))))


(defun $compile-livescript ()
  "Compile LiveScript to JavaScript"
  (interactive)
  (and (string-match ".*\.ls$" ($current-file-full-path))
       (compile (concat "livescript -c -d " ($current-file-full-path)))))

(switch-to-buffer "*scratch*")

(defun unicode-symbol (name)
  "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW
 or GREATER-THAN into an actual Unicode character code. "
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

(defun substitute-pattern-with-unicode (pattern symbol)
  "Add a font lock hook to replace the matched part of PATTERN
with the Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
  (font-lock-add-keywords
   nil `((,pattern
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(unicode-symbol symbol)
                                    'decompose-region)
                    nil))))))

(defun substitute-patterns-with-unicode (patterns)
  "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
  (mapcar #'(lambda (x)
              (substitute-pattern-with-unicode (car x)
                                               (cdr x)))
          patterns))

(defun tim-add-new-snippet (mode name)
  "Add new yassnipet snippet"
  (interactive "MMode (without the `-mode` part): \nMSnippet name: ")
  (let* ((mode (format "%s-mode" mode))
         (yas-file (format "~/.elisp/mysnippets/%s/%s" mode name)))
    (message "file: %s" yas-file)
    (find-file yas-file)))

(defun $add-license (license-file)
  "Add license to point"
  (interactive "sLicense (CaSE-SeNSitIVE): ")
  (insert-file (format "~/Docs/Licenses/%s.txt" license-file)))

;;;
;;; Wrappers, helpers
;;;

(defun $clipboard<-region (begin end)
  "Copy region to clipboard"
  (clipboard-kill-ring-save begin end))

(defun $kill-ring<- (str)
  "Copy a string to the kill ring"
  (interactive "MString: ")
  (kill-new str))

(defun $clipboard<- (str)
  "Copy a string to clipboard"
  (interactive "MString: ")
  (let ((x-select-enable-clipboard t))
    (x-select-text str)))

(defun $clipboard<-pwd ()
  "Copy current directory to clipboard"
  (interactive)
  ($clipboard<- (shell-command-to-string "pwd")))

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
  "Mark current line"
  (interactive)
  (beginning-of-line)
  (push-mark (point) t t)
  (end-of-line))

(defun $insert-me ()
  "Insert my information"
  (interactive)
  (insert "Dương \"Yang\" ヤン Nguyễn <cmpitg@gmail.com>"))

(defun $autoload-mode (file-regex mode-symbol)
  "Add autoload mode when opening file.
Example: ($autoload-mode \"Rakefile\" . 'ruby-mode)"
  (add-to-list 'auto-mode-alist (cons file-regex mode-symbol)))

;; (defun $make-test-command ()
;;   (interactive "")
;;   )

(defun $delete-line ()
  "Delete current line"
  (interactive)
  (beginning-of-line)
  (kill-line)
  (kill-line))

(defun $exec-in-other-window (command)
  "Execute in other window"
  (interactive "MCommand: ")
  (shell-command command))

(defun $exec-then-pipe (command)
  "Execute and pipe output to the current buffer"
  (interactive "MCommand: ")
  (shell-command command t))

(defun $pipe-then-exec (command)
  "Pipe current region to a command, exec it, and pipe the output back"
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
  "Toggle evil-mode for current buffer"
  (interactive)
  (if evil-local-mode
      (progn
        (evil-local-mode -1)
        (setq cursor-type 'bar))
    (evil-local-mode)))

(defun $auto-load-mode (filetypes mode)
  "Autoload mode for a list of filetypes"
  (dolist (filetype filetypes)
    (add-to-list 'auto-mode-alist (cons filetype mode))))

(defun $goto-snippets-folder ()
  "Go to personal snippets folder"
  (interactive)
  (find-file "~/.elisp/mysnippets"))

(defun $goto-keymap-ext-config ()
  "Go to keymap-extended configuration file"
  (interactive)
  (find-file "~/.elisp/emacs24/keymap-extended.el"))

(defun $goto-next-DEBUG ()
  "Go to next DEBUG"
  (interactive)
  (search-forward "DEBUG"))

(defun $goto-prev-DEBUG ()
  "Go to prev DEBUG"
  (interactive)
  (search-backward "DEBUG"))

(defun $goto-next-FIXME ()
  "Go to next FIXME"
  (interactive)
  (search-forward "FIXME"))

(defun $goto-prev-FIXME ()
  "Go to prev FIXME"
  (interactive)
  (search-backward "FIXME"))

(defun $defalias (new-symbol old-symbol doc-string)
  "Define an alias NEW-SYMBOL from OLD-SYMBOL with documentation
string.  E.g. ($defalias '$add-snippet 'tim-add-new-snippet \"Add
new snippet\")"
  (defalias new-symbol old-symbol)
  (puthash new-symbol doc-string *$doc-strings*))

($defalias '$filter-command '$pipe-then-exec
  "Filter a command")

($defalias '$pipe-then-exec-in-other-window 'shell-command-on-region
  "Filter a command but pipe the other to other window")

($defalias '$add-snippet 'tim-add-new-snippet
  "Add a new yasnippet snippet")

($defalias '$modify-opacity 'tim-opacity-modify
  "Modify current frame opacity")

($defalias '$toggle-ibus 'tim-ibus-toggle
  "Toogle ibus")

($defalias '$create-tags 'create-tags
  "Create tags table using exuberant-tags")

($defalias '$save-macro 'save-macro
  "Save current cached macro")

($defalias '$open-current-file-as-admin 'open-current-file-as-admin "")

($defalias '$delete-current-file 'delete-current-file "")

($defalias '$put-mode-line-to-top 'tim-put-mode-line-to-top "")

($defalias '$toggle-case 'tim-toggle-case "")

($defalias '$current-point 'point "")

($defalias '$current-word 'current-word "")

($defalias '$goto-point 'goto-char
  "Goto `point` in the buffer")

($defalias '$add-me '$insert-me
  "Insert me")

($defalias '$mark-defun 'mark-defun
  "mark-defun")

;;;
;;; Autoload
;;;

($autoload-mode "Rakefile" 'ruby-mode)

;;;
;;; Settings
;;;

(put 'font-lock-add-keywords 'lisp-indent-function 1)
(put '$defalias 'lisp-indent-function 2)
(put 'list 'lisp-indent-function nil)

(font-lock-add-keywords 'emacs-lisp-mode
  (list (cons (eval-when-compile
                (regexp-opt *tim-emacs-lisp-keywords* 'words))
              font-lock-keyword-face)
        (cons (eval-when-compile
                (regexp-opt *tim-emacs-lisp-functions* 'words))
              font-lock-function-name-face)))

(defun color-theme-textmate-modified ()
  "Color theme by hosiawak, created 2007-03-15."
  (interactive)
  (color-theme-install
   '(color-theme-textmate-modified
     ((background-color . "#162433")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "yellow")
      (foreground-color . "#C7D4E2")
      (mouse-color . "sienna1"))
     ;;(default ((t (:background "black" :foreground "white"))))
     (blue ((t (:foreground "blue"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "black" :foreground "white"))))
     (font-lock-builtin-face ((t (:foreground "white"))))
     (font-lock-comment-face ((t (:foreground "gray50"))))
     (font-lock-constant-face ((t (:foreground "#00CC00"))))
     (font-lock-doc-string-face ((t (:foreground "snow"))))
     (font-lock-doc-face ((t (:foreground "snow"))))
     (font-lock-function-name-face ((t (:foreground "tomato"))))
     (font-lock-keyword-face ((t (:foreground "#F9BB00"))))
     (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
     (font-lock-reference-face ((t (:foreground "SlateBlue"))))
     (font-lock-string-face ((t (:foreground "#00CC00"))))
     (font-lock-type-face ((t (:foreground "cornflower blue"))))
     (font-lock-variable-name-face ((t (:foreground "white"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
     (gui-element ((t (:background "#D4D0C8" :foreground "black"))))
     (highlight ((t (:background "darkolivegreen"))))
     (highline-face ((t (:background "SeaGreen"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil))))
     (underline ((nil (:underline nil))))
     (zmacs-region ((t (:background "snow" :foreground "ble")))))))

(provide 'color-theme-textmate-modified)

(color-theme-textmate-modified)

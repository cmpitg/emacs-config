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

(defun -load-files-if-exists- (&rest paths)
  "Load files when they exists."
  (dolist (file-path paths)
   (when (file-exists-p file-path)
     (load file-path))))

(add-to-list 'load-path "~/emacs-config/")
(add-to-list 'load-path "~/emacs-config/config-default/")

(-load-files-if-exists- "~/emacs-custom-foremost.el"  ; Foremost
                        "~/emacs-config/global-vars.el"
                        "~/emacs-config/package-list.el"
                        "~/emacs-config/init-package-managers.el"
                        "~/emacs-config/main.el"
                        "~/emacs-config/config-default/environment.el"
                        "~/emacs-custom.el"           ; User-defined customization
                        )

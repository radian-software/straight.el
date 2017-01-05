;;; straight.el --- The straightforward package manager.

;; Copyright (C) 2017 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Homepage: https://github.com/raxod502/straight.el
;; Keywords: extensions
;; Created: 1 Jan 2017

;;; Commentary:

;; Please see https://github.com/raxod502/straight.el for more
;; information.

;;; Code:

;; To see the outline of this file, run M-x occur with a query of four
;; semicolons followed by a space.

;; Wishful thinking API.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Selecting packages

(defun straight-select-installed-package ()
  (message "FIXME: actually select an installed package"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Building packages

(defun straight-symlink-package (package)
  (interactive (list (straight-select-installed-package)))
  (message "FIXME: actually symlink package"))

(defun straight-generate-package-autoloads (package)
  (interactive (list (straight-select-installed-package)))
  (message "FIXME: actually generate package autoloads"))

(defun straight-byte-compile-package (package)
  (interactive (list (straight-select-installed-package)))
  (message "FIXME: actually byte-compile package"))

(defun straight-build-package (package)
  (interactive (list (straight-select-installed-package)))
  (straight-symlink-package package)
  (straight-generate-package-autoloads package)
  (straight-byte-compile-package package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Activating packages

(defun straight-activate-package-autoloads (package)
  (interactive (list (straight-select-installed-package)))
  (message "FIXME: actually activate package autoloads"))

;; Code goes here.

(provide 'straight)

;;; straight.el ends here

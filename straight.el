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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Computing filesystem paths

(defvar straight-directory "straight")

(defun straight-directory ()
  (concat user-emacs-directory straight-directory))

(defvar straight-repo-directory "repo")

(defun straight-repo-directory ()
  (concat (straight-directory) straight-repo-directory))

(defvar straight-build-directory "build")

(defun straight-build-directory ()
  (concat (straight-directory) straight-build-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Selecting packages

(defun straight-select-installed-package ()
  (completing-read
   "Select package: "
   (directory-files (straight-build-directory) nil "^[^.]$")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Checking whether packages need to be rebuilt



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Building packages

(defun straight-generate-package-autoloads (package)
  (message "FIXME: actually generate package autoloads"))

(defun straight-byte-compile-package (package)
  (message "FIXME: actually byte-compile package"))

(defun straight-build-package (package)
  (straight-symlink-package package)
  (straight-generate-package-autoloads package)
  (straight-byte-compile-package package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Activating packages

(defun straight-activate-package-autoloads (package)
  (interactive (list (straight-select-installed-package)))
  (message "FIXME: actually activate package autoloads"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Closing remarks

(provide 'straight)

;;; straight.el ends here

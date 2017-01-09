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
;;;; Libraries

(require 'subr-x)
(require 'cl-lib)
(require 'package-build)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Utility functions

(defun straight--dir (&rest segments)
  (apply 'concat user-emacs-directory
         (mapcar (lambda (segment)
                   (concat segment "/"))
                 (cons "straight" segments))))

(defun straight--file (&rest segments)
  (substring (apply 'straight--dir segments) 0 -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Building packages

(defvar straight--build-cache nil)

;;;###autoload
(defun straight-load-cache ()
  (setq straight--build-cache
        (or (with-temp-buffer
              (insert-file-contents-literally
               (straight--file "cache.el"))
              (ignore-errors
                (read (current-buffer))))
            (make-hash-table :test 'equal))))

;;;###autoload
(defun straight-save-cache ()
  (with-temp-file (straight--file "cache.el")
    (pp straight--build-cache (current-buffer))))

(defmacro straight--with-build-recipe (build-recipe &rest body)
  (declare (indent 1))
  `(let* ((package (plist-get build-recipe :package))
          (repo (or (plist-get build-recipe :repo)
                    package))
          (files (plist-get build-recipe :files)))
     (unless package
       (error "Build recipe %S is missing :package" build-recipe))
     ,@body))

(defun straight-package-might-be-modified-p (build-recipe)
  (straight--with-build-recipe build-recipe
    (let ((mtime (gethash package straight--build-cache)))
      (or (not mtime)
          (with-temp-buffer
            (let ((default-directory (straight--dir "repos" repo)))
              (call-process
               "find" nil '(t t) nil
               "." "-name" ".git" "-o" "-newermt" mtime "-print")
              (> (buffer-size) 0)))))))

(defun straight--symlink-package (package repo files)
  (ignore-errors
    (delete-directory
     (straight--dir "build" package)
     'recursive))
  (make-directory (straight--dir "build" package) 'parents)
  (dolist (spec (package-build-expand-file-specs
                 (straight--dir "repos" repo)
                 (package-build--config-file-list `(:files ,files))))
    (let ((repo-file (straight--file "repos" repo (car spec)))
          (build-file (straight--file "build" package (cdr spec))))
      (unless (file-exists-p repo-file)
        (error "File %S does not exist" repo-file))
      (make-directory (file-name-directory build-file) 'parents)
      (make-symbolic-link repo-file build-file))))

(defun straight--autoload-file-name (package)
  (format "%s-autoloads.el" package))

(defun straight--generate-package-autoloads (package)
  (let ((generated-autoload-file
         (straight--file
          "build" package
          (straight--autoload-file-name package)))
        ;; Silence `autoload-generate-file-autoloads'.
        (noninteractive t))
    (ignore-errors
      (delete-file generated-autoload-file))
    (update-directory-autoloads
     (straight--dir "build" package))))

(defun straight--byte-compile-package (package)
  ;; Prevent Emacs from asking the user to save all their files before
  ;; compiling.
  (cl-letf (((symbol-function #'save-some-buffers) #'ignore))
    (byte-recompile-directory
     (straight--dir "build" package)
     0 'force)))

(defun straight--update-build-mtime (package)
  (let ((mtime (format-time-string "%FT%T%z")))
    (puthash package mtime straight--build-cache)))

(defun straight-build-package (build-recipe)
  (straight--with-build-recipe build-recipe
    (straight--symlink-package package repo files)
    (straight--generate-package-autoloads package)
    (straight--byte-compile-package package)
    (straight--update-build-mtime package)))

(defun straight-add-package-to-load-path (package)
  (add-to-list 'load-path (straight--dir "build" package)))

(defun straight-install-package-autoloads (package)
  (load (straight--file "build" package (straight--autoload-file-name package))
        nil 'nomessage))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MELPA

(defun straight--github-url (repo)
  (format "https://github.com/%s.git" repo))

(defun straight-ensure-melpa-is-cloned ()
  (unless (file-exists-p (straight--dir "repos/melpa"))
    (make-directory (straight--dir) 'parents)
    (let ((default-directory (straight--dir)))
      (unless (= 0 (call-process
                    "git" nil nil nil "clone"
                    (straight--github-url "melpa/melpa")))
        (error "error cloning MELPA")))))

(defun straight-get-melpa-recipe (package)
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents-literally
       (straight--file "repos/melpa/recipes" package))
      (read (current-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Temporary placeholder for high-level API

(defun straight-load-package (build-recipe)
  (straight--with-build-recipe build-recipe
    (when (straight-package-might-be-modified-p build-recipe)
      (straight-build-package build-recipe))
    (straight-add-package-to-load-path package)
    (straight-install-package-autoloads package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Closing remarks

(provide 'straight)

;;; straight.el ends here

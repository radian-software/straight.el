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
          (straight--autoload-file-name package))))
    (ignore-errors
      (delete-file generated-autoload-file))
    (let (;; Suppress messages about generating autoloads.
          (inhibit-message t)
          ;; Emacs seems to want to generate backup files otherwise.
          (backup-inhibited t)
          ;; This is in `package-generate-autoloads'. Presumably for a
          ;; good reason.
          (version-control 'never))
      (update-directory-autoloads
       (straight--dir "build" package))
      ;; And for some reason Emacs leaves a newly created buffer lying
      ;; around. Let's kill it.
      (when-let ((buf (find-buffer-visiting generated-autoload-file)))
        (kill-buffer buf)))))

(defun straight--byte-compile-package (package)
  (cl-letf (;; Prevent Emacs from asking the user to save all their
            ;; files before compiling.
            ((symbol-function #'save-some-buffers) #'ignore)
            ;; Die, byte-compile log, die!!!
            ((symbol-function #'byte-compile-log-1) #'ignore)
            ((symbol-function #'byte-compile-log-file) #'ignore)
            ((symbol-function #'byte-compile-log-warning) #'ignore))
    (let (;; Suppress messages about byte-compilation progress.
          (byte-compile-verbose nil)
          ;; Suppress messages about byte-compilation warnings.
          (byte-compile-warnings nil)
          ;; Suppress the remaining messages.
          (inhibit-message t))
      (byte-recompile-directory
       (straight--dir "build" package)
       0 'force))))

(defun straight--update-build-mtime (package)
  (let ((mtime (format-time-string "%FT%T%z")))
    (puthash package mtime straight--build-cache)))

(defun straight-build-package (build-recipe &optional nomsg)
  (straight--with-build-recipe build-recipe
    (unless nomsg
      (message "Building package %S..." package))
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
;;;; Managing repositories

(defvar straight--fetch-cache (make-hash-table :test 'equal))

(defun straight-register-repo (fetch-recipe)
  (let ((repo (plist-get fetch-recipe :repo)))
    (puthash repo fetch-recipe straight--fetch-cache)))

(defun straight--get-head (repo)
  (with-temp-buffer
    (let ((default-directory (straight--dir "repos" repo)))
      (unless (= 0 (call-process
                    "git" nil t nil  "rev-parse" "HEAD"))
        (error "Error checking HEAD of repo %S" repo)))
    (string-trim (buffer-string))))

(defun straight--set-head (repo head)
  (let ((default-directory (straight--dir "repos" repo)))
    (unless (= 0 (call-process
                  "git" nil nil nil "checkout" head))
      (error "Error performing checkout in repo %S" repo))))

(defun straight-save-versions ()
  (interactive)
  (with-temp-file (concat user-emacs-directory "straight/versions.el")
    (let ((versions nil))
      (maphash (lambda (repo fetch-recipe)
                 (push (cons repo (straight--get-head repo)) versions))
               straight--fetch-cache)
      (setq versions (cl-sort versions 'string-lessp :key 'car))
      (pp versions (current-buffer)))))

(defun straight-load-versions ()
  (interactive)
  (if-let ((versions (with-temp-buffer
                       (insert-file-contents-literally
                        (straight--file "versions.el"))
                       (ignore-errors
                         (read (current-buffer))))))
      (dolist (spec versions)
        (let ((repo (car spec))
              (head (cdr spec)))
          (straight--set-head repo head)))
    (error "Could not read from %S" (straight--file "versions.el"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Fetching repositories

(defun straight--github-url (repo)
  (format "https://github.com/%s.git" repo))

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
    (straight-register-repo `(:repo ,repo))
    (straight-add-package-to-load-path package)
    (when (straight-package-might-be-modified-p build-recipe)
      (straight-build-package build-recipe))
    (straight-install-package-autoloads package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Closing remarks

(provide 'straight)

;;; straight.el ends here

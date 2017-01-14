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
(require 'pbl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Utility functions

(defmacro straight--with-plist (plist props &rest body)
  (declare (indent 2))
  (let ((plist-sym (make-symbol "plist")))
    `(let* ((,plist-sym ,plist)
            ,@(mapcar (lambda (prop)
                        `(,prop
                          (plist-get
                           ,plist-sym
                           ,(intern (concat ":" (symbol-name prop))))))
                      props))
       ,@body)))

(defmacro straight--put (plist prop val)
  `(setq ,plist (plist-put ,plist ,prop ,val)))

(defun straight--dir (&rest segments)
  (expand-file-name
   (apply 'concat user-emacs-directory
          (mapcar (lambda (segment)
                    (concat segment "/"))
                  (cons "straight" segments)))))

(defun straight--file (&rest segments)
  (expand-file-name
   (substring (apply 'straight--dir segments) 0 -1)))

(defun straight--autoload-file-name (package)
  (format "%s-autoloads.el" package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Fetching repositories

(defun straight--repository-is-available-p (recipe)
  (straight--with-plist recipe
      (local-repo)
    (file-exists-p (straight--dir "repos" local-repo))))

(defun straight--clone-repository (recipe)
  (straight--with-plist recipe
      (local-repo)
    (message "Cloning repository %S..." local-repo)
    (pbl-checkout
     local-repo recipe
     (straight--dir "repos" local-repo))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Recipe processing

(defun straight--get-melpa-recipe (package)
  (unless (straight--repository-is-available-p melpa-repo-recipe)
    (straight--clone-repository melpa-repo-recipe))
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents-literally
         (straight--with-plist melpa-repo-recipe
             (local-repo)
           (straight--file "repos" local-repo "recipes" package)))
        (read (current-buffer)))
    (error (error "Could not find recipe for package %S in MELPA"
                  package))))

(defun straight--convert-recipe (melpa-recipe)
  (let ((melpa-recipe (if (listp melpa-recipe)
                          melpa-recipe
                        (straight--get-melpa-recipe
                         (symbol-name melpa-recipe)))))
    (cl-destructuring-bind (package . plist) melpa-recipe
      (straight--with-plist plist
          (local-repo repo)
        (let ((package (symbol-name package)))
          (straight--put plist :package package)
          (unless local-repo
            (straight--put plist :local-repo
                           (or (and repo
                                    (replace-regexp-in-string
                                     "^.+/" "" repo))
                               package)))
          plist)))))

(defvar melpa-repo-recipe (straight--convert-recipe
                           '(melpa :fetcher github
                                   :repo "melpa/melpa")))

(defvar straight--recipe-cache (make-hash-table :test 'equal))

(defun straight--register-recipe (recipe)
  (straight--with-plist recipe
      (package)
    (puthash package recipe straight--recipe-cache)))

(defun straight--map-repos (func)
  (let ((repos nil))
    (maphash (lambda (package recipe)
               (straight--with-plist recipe
                   (local-repo)
                 (unless (member repos local-repo)
                   (funcall func recipe)
                   (push local-repo repos))))
             straight--recipe-cache)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Managing repositories

(defun straight--update-package (recipe)
  ;; FIXME
  (error "Don't know how to update package"))

;; FIXME: handle VCS other than git
;; FIXME: handle validation
(defun straight--get-head (local-repo &optional validate)
  (with-temp-buffer
    (let ((default-directory (straight--dir "repos" local-repo)))
      (unless (= 0 (call-process
                    "git" nil t nil  "rev-parse" "HEAD"))
        (error "Error checking HEAD of repo %S" local-repo)))
    (string-trim (buffer-string))))

;; FIXME: handle VCS other than git
(defun straight--set-head (local-repo head)
  (let ((default-directory (straight--dir "repos" local-repo)))
    (unless (= 0 (call-process
                  "git" nil nil nil "checkout" head))
      (error "Error performing checkout in repo %S" local-repo))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Figuring out whether packages need to be rebuilt

(defvar straight--build-cache nil)

(defun straight--load-build-cache ()
  (setq straight--build-cache
        (or (with-temp-buffer
              (insert-file-contents-literally
               (straight--file "cache.el"))
              (ignore-errors
                (read (current-buffer))))
            (make-hash-table :test 'equal))))

(defun straight--save-build-cache ()
  (with-temp-file (straight--file "cache.el")
    (pp straight--build-cache (current-buffer))))

(defun straight--maybe-load-build-cache ()
  (if after-init-time
      (straight--load-build-cache)
    (unless (member #'straight--save-build-cache after-init-hook)
      (straight--load-build-cache))
    (add-hook 'after-init-hook #'straight--save-build-cache)))

(defun straight--maybe-save-build-cache ()
  (when after-init-time
    (straight--save-build-cache)))

(defun straight--package-might-be-modified-p (recipe)
  (straight--with-plist recipe
      (package local-repo)
    (let ((mtime (gethash package straight--build-cache)))
      (or (not mtime)
          (with-temp-buffer
            (let ((default-directory (straight--dir "repos" local-repo)))
              (call-process
               "find" nil '(t t) nil
               "." "-name" ".git" "-o" "-newermt" mtime "-print")
              (> (buffer-size) 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Building packages

(defun straight--symlink-package (recipe)
  (straight--with-plist recipe
      (package local-repo files)
    (let ((dir (straight--dir "build" package)))
      (when (file-exists-p dir)
        (delete-directory dir 'recursive)))
    (make-directory (straight--dir "build" package) 'parents)
    (dolist (spec (pbl-expand-file-specs
                   (straight--dir "repos" local-repo)
                   (pbl--config-file-list `(:files ,files))))
      (let ((repo-file (straight--file "repos" local-repo (car spec)))
            (build-file (straight--file "build" package (cdr spec))))
        (unless (file-exists-p repo-file)
          (error "File %S does not exist" repo-file))
        (make-directory (file-name-directory build-file) 'parents)
        (make-symbolic-link repo-file build-file)))))

(defun straight--generate-package-autoloads (recipe)
  (straight--with-plist recipe
      (package)
    (let ((generated-autoload-file
           (straight--file
            "build" package
            (straight--autoload-file-name package))))
      (when (file-exists-p generated-autoload-file)
        (delete-file generated-autoload-file))
      (let (;; The following bindings are in
            ;; `package-generate-autoloads'. Presumably for a good
            ;; reason.
            (noninteractive t)
            (backup-inhibited t)
            (version-control 'never)
            ;; Even so, Emacs just won't shut up unless we really tell
            ;; it to.
            (inhibit-message t))
        (update-directory-autoloads
         (straight--dir "build" package))
        ;; And for some reason Emacs leaves a newly created buffer
        ;; lying around. Let's kill it.
        (when-let ((buf (find-buffer-visiting generated-autoload-file)))
          (kill-buffer buf))))))

(defun straight--byte-compile-package (recipe)
  (straight--with-plist recipe
      (package)
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
         0 'force)))))

(defun straight--update-build-mtime (recipe)
  (straight--with-plist recipe
      (package)
    (let ((mtime (format-time-string "%FT%T%z")))
      (puthash package mtime straight--build-cache))))

(defun straight--build-package (recipe)
  (straight--with-plist recipe
      (package)
    (message "Building package %S..." package))
  (straight--symlink-package recipe)
  (straight--generate-package-autoloads recipe)
  (straight--byte-compile-package recipe)
  (straight--update-build-mtime recipe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Loading packages

(defun straight--add-package-to-load-path (recipe)
  (straight--with-plist recipe
      (package)
    (add-to-list 'load-path (straight--dir "build" package))))

(defun straight--install-package-autoloads (recipe)
  (straight--with-plist recipe
      (package)
    (load (straight--file "build" package (straight--autoload-file-name package))
          nil 'nomessage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; API

;;;###autoload
(defun straight-get-melpa-recipe (&optional action)
  (interactive (if current-prefix-arg
                   'copy
                 'insert))
  (unless (straight--repository-is-available-p melpa-repo-recipe)
    (straight--clone-repository melpa-repo-recipe))
  (let* ((package (completing-read
                   "Which recipe? "
                   (straight--with-plist melpa-repo-recipe
                       (repo)
                     (directory-files (straight--dir "repos" repo "recipes")))
                   (lambda (elt) t)
                   'require-match))
         (recipe (straight--get-melpa-recipe package)))
    (pcase action
      ('insert (insert (format "%S" recipe)))
      ('copy (kill-new (format "%S" recipe)))
      (_ recipe))))

;;;###autoload
(defun straight-use-package (melpa-recipe)
  (interactive (list (straight-get-melpa-recipe)))
  (let ((recipe (straight--convert-recipe melpa-recipe)))
    ;; FIXME: don't register recipe when interactive
    (straight--register-recipe recipe)
    (unless (straight--repository-is-available-p recipe)
      (straight--clone-repository recipe))
    (straight--add-package-to-load-path recipe)
    (straight--maybe-load-build-cache)
    (when (straight--package-might-be-modified-p recipe)
      (straight--build-package recipe))
    (straight--maybe-save-build-cache)
    (straight--install-package-autoloads recipe)))

;;;###autoload
(defun straight-update-package (package)
  (interactive (list (completing-read
                      "Update package: "
                      (hash-table-keys straight--recipe-cache)
                      (lambda (elt) t)
                      'require-match)))
  (straight--update-package (gethash package straight--recipe-cache)))

;;;###autoload
(defun straight-update-all ()
  (interactive)
  (straight--map-repos (lambda (recipe)
                         (straight--with-plist recipe
                             (package)
                           (straight-update-package
                            package)))))

;;;###autoload
(defun straight-save-versions ()
  (interactive)
  (with-temp-file (concat user-emacs-directory "straight/versions.el")
    (let ((versions nil))
      (straight--map-repos (lambda (recipe)
                             (straight--with-plist recipe
                                 (repo)
                               (push (cons repo (straight--get-head
                                                 repo 'validate))
                                     versions))))
      (setq versions (cl-sort versions 'string-lessp :key 'car))
      (pp versions (current-buffer)))))

;;;###autoload
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
;;;; Closing remarks

(provide 'straight)

;;; straight.el ends here

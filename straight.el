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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Lazy-load pbl

(autoload 'pbl-checkout "pbl")
(autoload 'pbl-expand-file-specs "pbl")
(autoload 'pbl--config-file-list "pbl")

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
  `(progn
     (setq ,plist (copy-sequence ,plist))
     (setq ,plist (plist-put ,plist ,prop ,val))))

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
     (straight--dir "repos" local-repo))
    (message "Cloning repository %S...done" local-repo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Recipe processing

(defvar gnu-elpa-url "git://git.savannah.gnu.org/emacs/elpa.git")

(defvar melpa-recipe)
(defvar gnu-elpa-recipe)

(defun straight--get-gnu-elpa-recipe (package)
  (unless (straight--repository-is-available-p gnu-elpa-recipe)
    (straight--clone-repository gnu-elpa-recipe))
  (straight--with-plist gnu-elpa-recipe
      (local-repo)
    (when (file-exists-p (straight--dir
                          "repos" local-repo "packages"
                          (symbol-name package)))
      `(,package :fetcher git
                 :url ,gnu-elpa-url
                 :files (,(format "packages/%s/*.el"
                                  (symbol-name package)))
                 :local-repo "elpa"))))

(defun straight--get-melpa-recipe (package)
  (unless (straight--repository-is-available-p melpa-recipe)
    (straight--clone-repository melpa-recipe))
  (with-temp-buffer
    (when
        (condition-case nil
            (insert-file-contents-literally
             (straight--with-plist melpa-recipe
                 (local-repo)
               (straight--file "repos" local-repo "recipes"
                               (symbol-name package))))
          (error nil))
      (read (current-buffer)))))

(defvar straight--repo-cache (make-hash-table :test 'equal))

(defvar straight--fetch-keywords
  '(:fetcher :url :repo :commit :branch :module))

(defvar straight--keywords
  '(:package :local-repo :files :fetcher :url :repo
    :commit :branch :module))

(defun straight--convert-recipe (melpa-style-recipe)
  (let* ((from-melpa-p nil)
         (full-melpa-style-recipe
          (if (listp melpa-style-recipe)
              melpa-style-recipe
            (or (when-let ((melpa-recipe
                            (straight--get-melpa-recipe
                             melpa-style-recipe)))
                  (setq from-melpa-p t)
                  melpa-recipe)
                (straight--get-gnu-elpa-recipe
                 melpa-style-recipe)
                (error (concat "Could not find package %S "
                               "in MELPA or GNU ELPA")
                       melpa-style-recipe)))))
    (cl-destructuring-bind (package . plist) full-melpa-style-recipe
      (straight--with-plist plist
          (local-repo repo url)
        (let ((package (symbol-name package)))
          (straight--put plist :package package)
          (unless local-repo
            (straight--put plist :local-repo
                           (or (when repo
                                 (replace-regexp-in-string
                                  "^.+/" "" repo))
                               (when (string-suffix-p ".git" url)
                                 (replace-regexp-in-string
                                  "^.*/\\(.+\\)\\.git$" "\\1" url))
                               package)))
          (when from-melpa-p
            (straight--with-plist plist
                (local-repo)
              (when-let (original-recipe (gethash local-repo
                                                  straight--repo-cache))
                (dolist (keyword straight--fetch-keywords)
                  (when-let ((value (plist-get original-recipe keyword)))
                    (straight--put plist keyword value))))))
          plist)))))

(setq melpa-recipe (straight--convert-recipe
                    '(melpa :fetcher github
                            :repo "melpa/melpa")))

(setq gnu-elpa-recipe (straight--convert-recipe
                       `(elpa :fetcher git
                              :url ,gnu-elpa-url)))

(defvar straight--recipe-cache (make-hash-table :test 'equal))

(defun straight--register-recipe (recipe)
  (straight--with-plist recipe
      (package local-repo)
    (when-let ((existing-recipe (gethash local-repo straight--repo-cache)))
      (dolist (keyword straight--fetch-keywords)
        (unless (equal (plist-get recipe keyword)
                       (plist-get existing-recipe keyword))
          (error "Packages %S and %S have incompatible recipes (%S cannot be both %S and %S)"
                 (plist-get existing-recipe :package)
                 (plist-get recipe :package)
                 keyword
                 (plist-get existing-recipe keyword)
                 (plist-get recipe keyword)))))
    (when-let ((existing-recipe (gethash package straight--recipe-cache)))
      (dolist (keyword straight--keywords)
        (unless (equal (plist-get recipe keyword)
                       (plist-get existing-recipe keyword))
          (error "Package %S has two incompatible recipes (%S cannot be both %S and %S)"
                 package
                 keyword
                 (plist-get existing-recipe keyword)
                 (plist-get recipe keyword)))))
    (puthash local-repo recipe straight--repo-cache)
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

(defun straight--get-recipe (package)
  (or (gethash package straight--recipe-cache)
      (straight--convert-recipe (intern package))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Managing repositories

(defun straight--update-package (recipe)
  ;; FIXME
  (error "Don't know how to update packages yet"))

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
              (ignore-errors
                (insert-file-contents-literally
                 (straight--file "build-cache.el"))
                (read (current-buffer))))
            (make-hash-table :test 'equal))))

(defun straight--save-build-cache ()
  (with-temp-file (straight--file "build-cache.el")
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
    (or (not (file-exists-p (straight--file "build" package)))
        (let ((mtime (car (gethash package straight--build-cache))))
          (or (not mtime)
              (with-temp-buffer
                (let ((default-directory (straight--dir "repos" local-repo)))
                  (call-process
                   "find" nil '(t t) nil
                   "." "-name" ".git" "-o" "-newermt" mtime "-print")
                  (> (buffer-size) 0))))))))

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

(defun straight--process-dependencies (dependencies)
  (eval-and-compile
    (require 'package))
  (let ((packages nil))
    (dolist (spec dependencies)
      (cl-destructuring-bind (package . version) spec
        (unless (package-built-in-p package)
          (push (symbol-name package) packages))))
    packages))

(defun straight--compute-dependencies (package)
  (let ((dependencies
         (or (condition-case nil
                 (with-temp-buffer
                   (insert-file-contents-literally
                    (straight--file
                     "build" package
                     (format "%s-pkg.el" package)))
                   (straight--process-dependencies
                    (eval (nth 4 (read (current-buffer))))))
               (error nil))
             (condition-case nil
                 (with-temp-buffer
                   (insert-file-contents-literally
                    (straight--file
                     "build" package
                     (format "%s.el" package)))
                   (re-search-forward "^;; Package-Requires: ")
                   (straight--process-dependencies
                    (read (current-buffer))))
               (error nil)))))
    (puthash package (cons (car (gethash package straight--build-cache))
                           dependencies)
             straight--build-cache)))

(defun straight--get-dependencies (package)
  (cdr (gethash package straight--build-cache)))

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
            ;; Tell Emacs to shut up.
            (message-log-max nil)
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
            (inhibit-message t)
            (message-log-max nil))
        (byte-recompile-directory
         (straight--dir "build" package)
         0 'force)))))

(defun straight--update-build-mtime (recipe)
  (straight--with-plist recipe
      (package)
    (let ((mtime (format-time-string "%FT%T%z")))
      (puthash package (cons mtime (cdr (gethash package
                                                 straight--build-cache)))
               straight--build-cache))))

(defun straight--build-package (recipe &optional interactive)
  (straight--with-plist recipe
      (package)
    (message "Building package %S..." package)
    (straight--symlink-package recipe)
    (straight--compute-dependencies package)
    (dolist (dependency (straight--get-dependencies package))
      (straight-use-package (straight--get-recipe dependency)
                            interactive 'straight-style))
    (message "Building package %S..." package)
    (straight--generate-package-autoloads recipe)
    (straight--byte-compile-package recipe)
    (straight--update-build-mtime recipe)
    (message "Building package %S...done" package)))

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
(defun straight-get-elpa-recipe (&optional action)
  (interactive (list (if current-prefix-arg
                         'copy
                       'insert)))
  (unless (straight--repository-is-available-p melpa-recipe)
    (straight--clone-repository melpa-recipe))
  (unless (straight--repository-is-available-p gnu-elpa-recipe)
    (straight--clone-repository gnu-elpa-recipe))
  (let* ((package (intern
                   (completing-read
                    "Which recipe? "
                    (sort
                     (delete-dups
                      (append
                       (straight--with-plist melpa-recipe
                           (local-repo)
                         (directory-files
                          (straight--dir "repos" local-repo "recipes")
                          nil "^[^.]" 'nosort))
                       (straight--with-plist gnu-elpa-recipe
                           (local-repo)
                         (directory-files
                          (straight--dir "repos" local-repo "packages")
                          nil "^[^.]" 'nosort))))
                     'string-lessp)
                    (lambda (elt) t)
                    'require-match)))
         (recipe (or (straight--get-melpa-recipe package)
                     (straight--get-gnu-elpa-recipe package))))
    (pcase action
      ('insert (insert (format "%S" recipe)))
      ('copy (kill-new (format "%S" recipe))
             (message "Copied \"%S\" to kill ring" recipe))
      (_ recipe))))

;;;###autoload
(defun straight-use-package (melpa-style-recipe
                             &optional interactive straight-style)
  (interactive (list (straight-get-elpa-recipe) t))
  (let ((recipe (if straight-style melpa-style-recipe
                  (straight--convert-recipe melpa-style-recipe))))
    (straight--with-plist recipe
        (package)
      (if (or after-init-time
              (not (gethash package
                            straight--recipe-cache)))
          (progn
            (unless interactive
              (straight--register-recipe recipe))
            (unless (straight--repository-is-available-p recipe)
              (straight--clone-repository recipe))
            (straight--add-package-to-load-path recipe)
            (straight--maybe-load-build-cache)
            (when (straight--package-might-be-modified-p recipe)
              (straight--build-package recipe interactive))
            (straight--maybe-save-build-cache)
            (straight--install-package-autoloads recipe)
            (dolist (dependency (straight--get-dependencies package))
              (straight-use-package (straight--get-recipe dependency)
                                    interactive 'straight-style))
            (when interactive
              (message
               (concat "If you want to keep %s, put "
                       "(straight-use-package %s%S) "
                       "in your init-file.")
               package "'" (intern package))))
        (unless interactive
          (straight--register-recipe recipe))))))

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
;;;; Mess with other packages

(setq package-enable-at-startup nil)

(with-eval-after-load 'use-package
  (defvar use-package-keywords)
  (defvar use-package-defaults)
  (defvar use-package-ensure-function)
  (declare-function use-package-only-one "use-package")
  (declare-function use-package-process-keywords "use-package")
  (declare-function use-package-as-symbol "use-package")
  (unless (member :recipe use-package-keywords)
    (setq use-package-keywords
          (let* ((pos (cl-position :ensure use-package-keywords))
                 (head (cl-subseq use-package-keywords 0 (+ 1 pos)))
                 (tail (cl-subseq use-package-keywords (+ 1 pos))))
            (append head (list :recipe) tail))))
  (defun use-package-normalize/:recipe (name-symbol keyword args)
    (use-package-only-one (symbol-name keyword) args
      (lambda (label arg)
        (unless (listp arg)
          (error ":recipe wants a list"))
        (if (keywordp (car arg))
            (cons name-symbol arg)
          arg))))
  (defun use-package-handler/:recipe (name keyword recipe rest state)
    (let* ((body (use-package-process-keywords name rest state))
           (ensure-form `(straight-use-package
                          ',(or recipe
                                (use-package-as-symbol name)))))
      (if (bound-and-true-p byte-compile-current-file)
          (eval ensure-form)
        (push ensure-form body))
      body))
  (unless (assoc :recipe use-package-defaults)
    (push '(:recipe name-symbol t) use-package-defaults))
  (setq use-package-ensure-function #'ignore))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Closing remarks

(provide 'straight)

;;; straight.el ends here

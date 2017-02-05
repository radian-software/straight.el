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

(defmacro straight--with-progress (task &rest body)
  (declare (indent 1))
  (let ((task-sym (make-symbol "task")))
    `(let ((,task-sym ,task))
       (prog2
           (message "%s..." ,task-sym)
           (progn
             ,@body)
         (message "%s...done" ,task-sym)))))

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

(defun straight--insert (n key value table)
  (let ((list (gethash key table)))
    (if (>= n (length list))
        (puthash key
                 (append list
                         (make-list (- n (length list)) nil)
                         (list value))
                 table)
      (setcar (nthcdr n list) value))
    table))

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

(defun straight--clone-repository (recipe &optional parent-recipe relation)
  (straight--with-plist recipe
      (package local-repo)
    (straight--with-progress
        (if parent-recipe
            (format "Cloning repository %S for package %S (%s %S)"
                    local-repo package (or relation "dependency of")
                    (plist-get parent-recipe :package))
          (format "Cloning repository %S for package %S"
                  local-repo package))
      (pbl-checkout
       local-repo recipe
       (straight--dir "repos" local-repo)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Recipe processing

(defvar gnu-elpa-url "git://git.savannah.gnu.org/emacs/elpa.git")

(defvar melpa-recipe)
(defvar gnu-elpa-recipe)
(defvar emacsmirror-recipe)

(defun straight--get-gnu-elpa-recipe (package)
  (unless (straight--repository-is-available-p gnu-elpa-recipe)
    (straight--clone-repository gnu-elpa-recipe
                                `(:package ,(symbol-name package))
                                "looking for"))
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
    (straight--clone-repository melpa-recipe
                                `(:package ,(symbol-name package))
                                "looking for"))
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

(defun straight--get-emacsmirror-recipe (package)
  (unless (straight--repository-is-available-p emacsmirror-recipe)
    (straight--clone-repository emacsmirror-recipe
                                `(:package ,(symbol-name package))
                                "looking for"))
  (straight--with-plist emacsmirror-recipe
      (local-repo)
    (let ((default-directory (straight--dir "repos" local-repo)))
      (with-temp-buffer
        (when (= 0 (call-process
                    "git" nil t nil "config" "-f" ".gitmodules"
                    "--get" (format "submodule.%s.url"
                                    (symbol-name package))))
          (let ((url (string-trim (buffer-string))))
            (if (string-match "^git@github\\.com:\\([A-Za-z0-9_.-]+\\)$" url)
                `(,package :fetcher github
                           :repo (match-string 1 url))
              `(,package :fetcher git
                         :url url))))))))

(defvar straight--repo-cache (make-hash-table :test 'equal))

(defvar straight--fetch-keywords
  '(:fetcher :url :repo :commit :branch :module))

(defvar straight--keywords
  '(:package :local-repo :files :fetcher :url :repo
    :commit :branch :module))

(defun straight--convert-recipe (melpa-style-recipe)
  (let* ((recipe-specified-p (listp melpa-style-recipe))
         (full-melpa-style-recipe
          (if recipe-specified-p
              melpa-style-recipe
            (let ((package melpa-style-recipe))
              (or
               (straight--get-melpa-recipe package)
               (straight--get-gnu-elpa-recipe package)
               (straight--get-emacsmirror-recipe package)
               (error (concat "Could not find package %S "
                              "in MELPA, GNU ELPA, or "
                              "Emacsmirror")
                      package))))))
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
                               ;; The following is a half-hearted
                               ;; attempt to turn arbitrary URLs into
                               ;; repository names.
                               (let ((regexp "^.*/\\(.+\\)\\.git$"))
                                 (when (string-match regexp url)
                                   (match-string 1 url)))
                               package)))
          ;; This code is here to deal with complications that can
          ;; arise with manual recipe specifications when multiple
          ;; packages are versioned in the same repository.
          ;;
          ;; Specifically, let's suppose packages `swiper' and `ivy'
          ;; are both versioned in repository "swiper", and let's
          ;; suppose that I load both of them in my init-file (`ivy'
          ;; first and then `swiper'). Now suppose that I discover a
          ;; bug in `ivy' and fix it in my fork, so that (until my fix
          ;; is merged) I need to provide an explicit recipe in my
          ;; init-file's call to `straight-use-package' for `ivy', in
          ;; order to use my fork. That will cause a conflict, because
          ;; the recipe for `swiper' is automatically taken from
          ;; MELPA, and it does not point at my fork, but instead at
          ;; the official repository. To fix the problem, I would have
          ;; to specify my fork in the recipe for `swiper' (and also
          ;; `counsel', a third package versioned in the same
          ;; repository). That violates DRY and is a pain.
          ;;
          ;; Instead, this code makes it so that if a recipe has been
          ;; automatically retrieved from a package source (for
          ;; example, MELPA, GNU ELPA, or Emacsmirror), and the
          ;; `:local-repo' specified in that recipe has already been
          ;; used for another package, then the configuration for that
          ;; repository will silently be copied over, and everything
          ;; should "just work".
          (unless recipe-specified-p
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

(setq emacsmirror-recipe (straight--convert-recipe
                          `(epkgs :fetcher github
                                  :repo "emacsmirror/epkgs"
                                  :nonrecursive t)))

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
  ;; You might think that this method could be eliminated entirely.
  ;; Instead, we could perform this defaulting behavior all inside
  ;; `straight--convert-recipe', you would say.
  ;;
  ;; It turns out that's not such a good idea, because then we aren't
  ;; able to distinguish between top-level packages and dependencies.
  ;; If the user specifies two incompatible recipes for a package, we
  ;; want to signal an error. However, if the user specifies one
  ;; recipe and a recipe repository provides another for a dependency
  ;; package, we want to silently reconcile the difference. This can
  ;; be done by having the dependency resolution functions call this
  ;; defaulting function, and then having `straight--convert-recipe'
  ;; just throw an error on a conflict (except for the repository
  ;; fetch part of the recipe, see the large comment in
  ;; `straight--convert-recipe' for details).
  (or (gethash package straight--recipe-cache)
      (straight--convert-recipe (intern package))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Managing repositories

(defun straight--get-vcs (&optional directory)
  (let ((directory (or directory default-directory)))
    (cond
     ((file-exists-p ".git") :git)
     ((file-exists-p ".hg") :mercurial)
     ((file-exists-p ".bzr") :bazaar)
     ((file-exists-p ".svn") :subversion)
     ((file-exists-p "CVS") :cvs)
     ((or (file-exists-p ".fslckout")
          (file-exists-p "_FOSSIL_"))
      :fossil)
     ((file-exists-p "_darcs") :darcs)
     (t nil))))

(defun straight--update-package (recipe)
  (error "Don't know how to update packages yet"))

(defun straight--get-head (local-repo &optional validate)
  (let ((default-directory (straight--dir "repos" local-repo)))
    (pcase (straight--get-vcs)
      (:git (let ((head (with-temp-buffer
                          (unless (= 0 (call-process
                                        "git" nil t nil  "rev-parse" "HEAD"))
                            (error "Error checking HEAD of repo %S" local-repo))
                          (string-trim (buffer-string)))))
              (when validate
                (warn "Don't know how to validate"))
              head))
      ('nil (ignore
             (when validate
               (display-warning
                'straight
                (concat "Repository %S is not version-controlled, "
                        "cannot verify HEAD is reachable from remote")))))
      (vcs (ignore
            (when validate
              (display-warning
               'straight
               (concat "Repository %S is version-controlled by VCS "
                       (symbol-name vcs) ", cannot verify HEAD is "
                       "reachable from remote"))))))))

(defun straight--set-head (local-repo head)
  (let ((default-directory (straight--dir "repos" local-repo)))
    (pcase (straight--get-vcs)
      (:git (unless (= 0 (call-process
                          "git" nil nil nil "checkout" head))
              (error "Error performing checkout in repo %S" local-repo)))
      ('nil (ignore
             (display-warning
              'straight
              (concat "Repository %S is not version-controlled, "
                      "cannot set HEAD"))))
      (vcs (ignore
            (display-warning
             'straight
             (concat "Repository %S is version-controlled by VCS "
                     (symbol-name vcs) ", cannot set HEAD")))))))

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
  (dolist (package (hash-table-keys straight--build-cache))
    (unless (gethash package straight--recipe-cache)
      (remhash package straight--build-cache)))
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

(defvar straight--cached-packages-might-be-modified-p :unknown)

(defun straight--cached-packages-might-be-modified-p ()
  (let ((repos nil)
        (args nil))
    (maphash
     (lambda (package build-info)
       (cl-destructuring-bind (mtime dependencies local-repo) build-info
         (unless (member local-repo repos)
           (push local-repo repos)
           (setq args (append (list "-o"
                                    "-path"
                                    (format "./%s/*" local-repo)
                                    "-newermt"
                                    mtime
                                    "-print")
                              args)))))
     straight--build-cache)
    (setq args (append (list "." "-depth" "2" "-name" ".git" "-prune")
                       args))
    (with-temp-buffer
      (let ((default-directory (straight--dir "repos")))
        (apply 'call-process "find" nil '(t t) nil args)
        (> (buffer-size) 0)))))

(defun straight--package-might-be-modified-p (recipe)
  (straight--with-plist recipe
      (package local-repo)
    (when (equal straight--cached-packages-might-be-modified-p
                 :unknown)
      (setq straight--cached-packages-might-be-modified-p
            (straight--cached-packages-might-be-modified-p)))
    (when (or straight--cached-packages-might-be-modified-p
              (not (gethash package straight--build-cache)))
      (or (not (file-exists-p (straight--file "build" package)))
          (let ((mtime (nth 0 (gethash package straight--build-cache))))
            (or (not mtime)
                (with-temp-buffer
                  (let ((default-directory (straight--dir "repos" local-repo)))
                    (call-process
                     "find" nil '(t t) nil
                     "." "-name" ".git" "-prune" "-o" "-newermt" mtime "-print")
                    (> (buffer-size) 0)))))))))

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
    (straight--insert 1 package dependencies straight--build-cache)))

(defun straight--get-dependencies (package)
  (nth 1 (gethash package straight--build-cache)))

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
      (package local-repo)
    (let ((mtime (format-time-string "%FT%T%z")))
      (straight--insert 0 package mtime straight--build-cache))
    (straight--insert 2 package local-repo straight--build-cache)))

(defun straight--build-package (recipe &optional interactive parent-recipe)
  (straight--with-plist recipe
      (package)
    (straight--with-progress (if parent-recipe
                                 (format "Building package %S (dependency of %S)"
                                         package (plist-get parent-recipe :package))
                               (format "Building package %S" package))
      (straight--symlink-package recipe)
      (straight--compute-dependencies package)
      (when-let ((dependencies (straight--get-dependencies package)))
        (dolist (dependency dependencies)
          (straight-use-package (straight--get-recipe dependency)
                                interactive 'straight-style
                                recipe))
        (if parent-recipe
            (message (concat "Finished checking dependencies, building "
                             "package %S (dependency of %S)")
                     package (plist-get parent-recipe :package))
          (message "Finished checking dependencies, building package %S..."
                   package)))
      (straight--generate-package-autoloads recipe)
      (straight--byte-compile-package recipe)
      (straight--update-build-mtime recipe))))

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
                             &optional
                             interactive straight-style
                             parent-recipe)
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
              (straight--clone-repository recipe parent-recipe))
            (straight--add-package-to-load-path recipe)
            (straight--maybe-load-build-cache)
            (when (straight--package-might-be-modified-p recipe)
              (straight--build-package recipe interactive))
            (straight--maybe-save-build-cache)
            (straight--install-package-autoloads recipe)
            (dolist (dependency (straight--get-dependencies package))
              (straight-use-package (straight--get-recipe dependency)
                                    interactive 'straight-style
                                    recipe))
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

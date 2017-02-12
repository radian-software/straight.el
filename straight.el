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

;;; Implementation notes:

;; straight.el has a couple of important abstractions to be aware of.
;; FIXME: dummy line to satisfy auto-fill-mode
;;
;; A "repository" or "repo" is a folder in ~/.emacs.d/straight/repos.
;;
;; It is usually a Git repository but it doesn't have to be. (Git
;; repositories are the only type of repositories that straight.el
;; knows how to manage intelligently, meaning update, validate, and so
;; on, so straight.el prefers to use Git repositories when possible,
;; but there is no restriction imposed on the version control system
;; you use, if any.)
;;
;; A repository generally has an upstream source specified in the
;; relevant "package recipe" (more on that later), but again there is
;; no restriction imposed on that: your repositories can just be
;; folders of Elisp files that you create manually without version
;; control, if for some reason you want that.
;;
;; A "package" is a collection of one or more Elisp files.
;;
;; Almost always, one of the files in a package will have a
;; specially-formatted header identifying its name, version, authors,
;; and other information. Other package managers, like package.el and
;; quelpa, require this header. straight.el does not. The only thing
;; that straight.el checks about the header is the dependency list,
;; and if this is missing it is assumed that the package has no
;; dependencies.
;;
;; There is not a one-to-one relationship between packages and
;; repositories. Normally, a repository will provide the Elisp files
;; for one and only one package, but this is not required. A single
;; repository can provide multiple packages, and there is no
;; requirement for a repository to provide any packages.
;;
;; A package is defined implicitly by its recipe. This idea is taken
;; from MELPA and tweaked a bit. In straight.el, a package recipe is a
;; property list (check the Elisp manual to read more about these)
;; with a number of possible keys:
;;
;; :package - required, the name of the package as a string (two
;;            packages cannot have the same name)
;;
;; :local-repo - required, the name of the repository

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Libraries

;; For `if-let', `when-let', `hash-table-keys', `string-join',
;; `string-trim', etc.
(require 'subr-x)

;; For `cl-destructuring-bind', `cl-some', `cl-letf', `cl-position',
;; `cl-subseq', etc.
(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Lazy-load pbl

;; Normally we don't autoload things explicitly. Instead, autoload
;; cookies are placed before each function we want to autoload.
;; However, `pbl' is a dependency of `straight', and `straight' is
;; what provides the autoload cookie processing functionality. So this
;; is really the only elegant way to lazy-load `pbl' (we do not want
;; to load it unless a package needs to be built), at least for now.
(autoload 'pbl-checkout "pbl")
(autoload 'pbl-expand-file-specs "pbl")
(autoload 'pbl--config-file-list "pbl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Customization

(defgroup straight nil
  "The straightforward package manager for Emacs."
  :group 'applications
  :prefix "straight-")

(defcustom straight-versions-file
  (locate-user-emacs-file "straight/versions.el")
  "The path to save versions.el in."
  :type 'file
  :group 'straight)

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

(defun straight--check-call (command &rest args)
  (= 0 (apply #'call-process command nil nil nil args)))

(defun straight--ensure-call (command &rest args)
  (unless (apply #'straight--check-call command args)
    (error "Command failed: %s %s"
           command (string-join args " "))))

(defun straight--get-call (command &rest args)
  (with-temp-buffer
    (unless (= 0 (apply #'call-process command
                        nil '(t t) nil args))
      (error "Command failed: %s %s"
             command (string-join args " ")))
    (string-trim (buffer-string))))

(defun straight--call-has-output-p (command &rest args)
  (with-temp-buffer
    (apply #'straight--check-call command args)
    (> (buffer-size) 0)))

(defun straight--warn (message &rest args)
  (ignore
   (display-warning 'straight (apply #'format message args))))

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
      (let ((url (condition-case nil
                     (straight--get-call
                      "git" "config" "-f" ".gitmodules"
                      "--get" (format "submodule.%s.url"
                                      (symbol-name package)))
                   (error nil))))
        (and (not (string-empty-p url))
             (if (string-match
                  (concat "^git@github\\.com:\\([A-Za-z0-9_.-]+"
                          "/[A-Za-z0-9_.-]+\\)\\.git$")
                  url)
                 `(,package :fetcher github
                            :repo ,(match-string 1 url))
               `(,package :fetcher git
                          :url ,url)))))))

(defvar straight--repo-cache (make-hash-table :test 'equal))

(defvar straight--fetch-keywords
  '(:fetcher :url :repo :commit :branch :module))

(defvar straight--keywords
  '(:package :local-repo :files :fetcher :url :repo
    :commit :branch :module))

(defun straight--lookup-recipe (package &optional sources)
  ;; We want to prefer Git, since that's the only VCS currently
  ;; supported. So we prefer MELPA recipes, but only if they are Git
  ;; repos, and then fall back to GNU ELPA and then Emacsmirror, and
  ;; as a last resort, non-Git MELPA recipes.
  (let ((melpa-recipe (and (member 'melpa sources)
                           (straight--get-melpa-recipe package))))
    (if (member (plist-get (cdr melpa-recipe) :fetcher)
                '(git github))
        melpa-recipe
      (or (and (member 'gnu-elpa sources)
               (straight--get-gnu-elpa-recipe package))
          (and (member 'emacsmirror sources)
               (straight--get-emacsmirror-recipe package))
          melpa-recipe
          (error (concat "Could not find package %S "
                         "in MELPA, GNU ELPA, or "
                         "Emacsmirror")
                 package)))))

(defun straight--convert-recipe (melpa-style-recipe)
  (let* ((recipe-specified-p (listp melpa-style-recipe))
         (full-melpa-style-recipe
          (if recipe-specified-p
              melpa-style-recipe
            (let ((package melpa-style-recipe))
              (straight--lookup-recipe package)))))
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
                                 (when (and url (string-match regexp url))
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
                 (unless (member local-repo repos)
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

(defun straight--version-controlled-p ()
  (file-exists-p ".git"))

(defun straight--fetch-remotes ()
  (straight--ensure-call "git" "fetch" "--all" "--tags"
                         "--recurse-submodules"))

(defun straight--get-branch ()
  (when-let ((symbolic-ref (condition-case nil
                               (straight--get-call
                                "git" "symbolic-ref" "HEAD")
                             (error nil))))
    (when (string-match "^refs/heads/\\(.+\\)" symbolic-ref)
      (match-string 1 symbolic-ref))))

(defun straight--get-upstream (branch)
  (or (condition-case nil
          (straight--get-call
           "git" "rev-parse" "--abbrev-ref"
           "--symbolic-full-name" "@{u}")
        (error nil))
      ;; FIXME: does this cover all the cases? We might want to look
      ;; at pushRemote and pushDefault config options.
      (with-temp-buffer
        (straight--ensure-call "git" "remote")
        (let ((remotes (split-string (buffer-string))))
          (unless (cdr remotes)
            (let ((remote (car remotes)))
              (erase-buffer)
              (call-process "git" nil t nil "branch" "-r")
              (goto-char (point-min))
              (let ((upstream (format "%s/%s" remote branch)))
                (when (re-search-forward upstream nil 'noerror)
                  upstream))))))))

(defun straight--is-ancestor (ancestor descendant)
  (straight--check-call "git" "merge-base" "--is-ancestor"
                        ancestor descendant))

(defun straight--merge-with-upstream (upstream)
  (straight--ensure-call "git" "merge" upstream))

(defun straight--update-package (recipe)
  (straight--with-plist recipe
      (local-repo)
    (let ((default-directory (straight--dir "repos" local-repo)))
      (if (straight--version-controlled-p)
          (progn
            (straight--fetch-remotes)
            (if-let ((branch (straight--get-branch)))
                (if-let ((upstream (straight--get-upstream branch)))
                    (if (straight--is-ancestor branch upstream)
                        ;; Local branch is behind upstream, merge.
                        (straight--merge-with-upstream upstream)
                      (unless (straight--is-ancestor upstream branch)
                        ;; Local branch has diverged from upstream,
                        ;; but is not purely ahead. Warn.
                        (straight--warn
                         (concat "In repository %S, cannot merge branch %S with "
                                 "upstream %S without merge or rebase")
                         local-repo branch upstream)))
                  (straight--warn
                   (concat "In repository %S, current branch %S has "
                           "no upstream, cannot update")
                   local-repo branch))
              (straight--warn
               "In repository %S, HEAD is detached, cannot update"
               local-repo)))
        (straight--warn
         (concat "Repository %S is not version-controlled with Git, "
                 "cannot update")
         local-repo)))))

(defun straight--get-head (local-repo)
  (let ((default-directory (straight--dir "repos" local-repo)))
    (when (straight--version-controlled-p)
      (straight--get-call "git" "rev-parse" "HEAD"))))

(defun straight--validate-head-is-reachable (local-repo head remote-urls)
  (let ((default-directory (straight--dir "repos" local-repo)))
    (cl-some (lambda (remote-branch)
               (when (string-match "^\\(.+?\\)/\\(.+\\)$" remote-branch)
                 (let ((remote (match-string 1 remote-branch))
                       (branch (match-string 2 remote-branch)))
                   (and (member (straight--get-call
                                 "git" "remote" "get-url" remote)
                                remote-urls)
                        (straight--is-ancestor
                         head remote-branch)))))
             (split-string (straight--get-call "git" "branch" "-r")))))

(defun straight--set-head (local-repo head)
  (let ((default-directory (straight--dir "repos" local-repo)))
    (if (straight--version-controlled-p)
        (unless (or (equal head (straight--get-head local-repo))
                    (straight--check-call "git" "checkout" head))
          (straight--warn "Could not perform checkout in repo %S" local-repo))
      (straight--warn
       (concat "Repository %S is not version-controlled with Git, "
               "cannot set HEAD")
       local-repo))))

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
    (when (and (not after-init-time)
               (equal straight--cached-packages-might-be-modified-p
                      :unknown))
      (setq straight--cached-packages-might-be-modified-p
            (straight--cached-packages-might-be-modified-p)))
    (when (or after-init-time
              straight--cached-packages-might-be-modified-p
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
;;;; Interactive helpers

(defun straight--select-package (message)
  (completing-read
   (concat message ": ")
   (hash-table-keys straight--recipe-cache)
   (lambda (elt) t)
   'require-match))

(defun straight--for-all-packages (func)
  (straight--map-repos
   (lambda (recipe)
     (straight--with-plist recipe
         (package)
       (funcall func package)))))

(defun straight--get-recipe-interactively (sources &optional action)
  (when (and (member 'melpa sources)
             (not (straight--repository-is-available-p melpa-recipe)))
    (straight--clone-repository melpa-recipe))
  (when (and (member 'gnu-elpa sources)
             (not (straight--repository-is-available-p gnu-elpa-recipe)))
    (straight--clone-repository gnu-elpa-recipe))
  (when (and (member 'emacsmirror sources)
             (not (straight--repository-is-available-p emacsmirror-recipe)))
    (straight--clone-repository emacsmirror-recipe))
  (let* ((package (intern
                   (completing-read
                    "Which recipe? "
                    (sort
                     (delete-dups
                      (append
                       (when (member 'melpa sources)
                         (straight--with-plist melpa-recipe
                             (local-repo)
                           (directory-files
                            (straight--dir "repos" local-repo "recipes")
                            nil "^[^.]" 'nosort)))
                       (when (member 'gnu-elpa sources)
                         (straight--with-plist gnu-elpa-recipe
                             (local-repo)
                           (directory-files
                            (straight--dir "repos" local-repo "packages")
                            nil "^[^.]" 'nosort)))
                       (when (member 'emacsmirror sources)
                         (straight--with-plist emacsmirror-recipe
                             (local-repo)
                           (append
                            (directory-files
                             (straight--dir "repos" local-repo "mirror")
                             nil "^[^.]" 'nosort)
                            (directory-files
                             (straight--dir "repos" local-repo "attic")
                             nil "^[^.]" 'nosort))))))
                     'string-lessp)
                    (lambda (elt) t)
                    'require-match)))
         (recipe (straight--lookup-recipe package sources)))
    (pcase action
      ('insert (insert (format "%S" recipe)))
      ('copy (kill-new (format "%S" recipe))
             (message "Copied \"%S\" to kill ring" recipe))
      (_ recipe))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; API

;;;###autoload
(defun straight-get-recipe (&optional action)
  ;; FIXME: don't name this almost the same as the totally unrelated
  ;; `straight--get-recipe-interactively'.
  (interactive (list (if current-prefix-arg
                         'copy
                       'insert)))
  (straight--get-recipe-interactively '(melpa gnu-elpa emacsmirror) action))

;;;###autoload
(defun straight-get-gnu-elpa-recipe (&optional action)
  (interactive (list (if current-prefix-arg
                         'copy
                       'insert)))
  (straight--get-recipe-interactively '(gnu-elpa) action))

;;;###autoload
(defun straight-get-melpa-recipe (&optional action)
  (interactive (list (if current-prefix-arg
                         'copy
                       'insert)))
  (straight--get-recipe-interactively '(melpa) action))

;;;###autoload
(defun straight-get-emacsmirror-recipe (&optional action)
  (interactive (list (if current-prefix-arg
                         'copy
                       'insert)))
  (straight--get-recipe-interactively '(emacsmirror) action))

;;;###autoload
(defun straight-use-package (melpa-style-recipe
                             &optional
                             interactive straight-style
                             parent-recipe reload)
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
            (when (and interactive (not reload))
              (message
               (concat "If you want to keep %s, put "
                       "(straight-use-package %s%S) "
                       "in your init-file.")
               package "'" (intern package))))
        (unless interactive
          (straight--register-recipe recipe))))))

;;;###autoload
(defun straight-reload-package (package &optional interactive)
  (interactive (list (straight--select-package "Reload package")
                     'interactive))
  (straight-use-package (gethash package straight--recipe-cache)
                        interactive 'straight-style nil 'reload))

;;;###autoload
(defun straight-reload-all ()
  (straight--for-all-packages #'straight-reload-package))

;;;###autoload
(defun straight-update-package (package)
  (interactive (list (straight--select-package "Update package")))
  (straight--with-progress (format "Updating package %S" package)
    (straight--update-package (gethash package straight--recipe-cache))))

;;;###autoload
(defun straight-update-all ()
  (interactive)
  (straight--for-all-packages #'straight-update-package))

;;;###autoload
(defun straight-validate-package (package &optional nomsg)
  (interactive (list (straight--select-package "Validate package")))
  (straight--with-plist (gethash package straight--recipe-cache)
      (local-repo fetcher repo url)
    (let ((default-directory (straight--dir "repos" local-repo)))
      (if (straight--version-controlled-p)
          (if-let ((head (straight--get-head local-repo)))
              (if-let ((remote-urls
                        (pcase fetcher
                          ('git (list url))
                          ('github
                           (list
                            (format "https://github.com/%s.git" repo)
                            (format "git@github.com:%s.git" repo))))))
                  (let ((valid-p (straight--validate-head-is-reachable
                                  local-repo head remote-urls)))
                    (if valid-p
                        (unless nomsg
                          (message "Package %S is all good" package))
                      (straight--warn
                       "HEAD of repository %S%s is not reachable from remote"
                       local-repo
                       (if-let ((branch (straight--get-branch)))
                           (format " (on branch %S)" branch)
                         "")))
                    valid-p)
                (straight--warn "Repository %S uses non-Git fetcher `%S'"
                                local-repo fetcher))
            (straight--warn "Repository %S has a detached HEAD" local-repo))
        (straight--warn "Repository %S is not version-controlled with Git")))))

;;;###autoload
(defun straight-validate-all (&optional nomsg)
  (interactive)
  (let ((valid-repos 0)
        (total-repos 0))
    (straight--map-repos
     (lambda (recipe)
       (straight--with-plist recipe
           (package)
         (when (straight-validate-package
                package 'nomsg)
           (setq valid-repos (1+ valid-repos)))
         (setq total-repos (1+ total-repos)))))
    (cond
     ((zerop total-repos)
      (user-error "No packages loaded"))
     ((zerop valid-repos)
      (unless nomsg
        (message "All %d packages have unreachable HEADS" total-repos)))
     ((= valid-repos total-repos)
      (unless nomsg
        (message "All %d packages have reachable HEADS" total-repos)))
     (t
      (unless nomsg
        (message "%d packages have reachable HEADS, %d packages do not"
                 valid-repos (- total-repos valid-repos)))))
    (= valid-repos total-repos)))

;;;###autoload
(defun straight-save-versions (&optional force)
  (interactive)
  (and (or force
           (straight-validate-all 'nomsg)
           (ignore
            (message
             "Not all packages have reachable HEADS, aborting")))
       (with-temp-file straight-versions-file
         (let ((versions nil))
           (straight--map-repos (lambda (recipe)
                                  (straight--with-plist recipe
                                      (local-repo)
                                    (push (cons local-repo
                                                (straight--get-head
                                                 local-repo))
                                          versions))))
           (setq versions (cl-sort versions 'string-lessp :key 'car))
           (pp versions (current-buffer)))
         (message "Wrote %s" straight-versions-file))))

;;;###autoload
(defun straight-load-versions ()
  (interactive)
  (if-let ((versions (with-temp-buffer
                       (insert-file-contents-literally
                        straight-versions-file)
                       (ignore-errors
                         (read (current-buffer))))))
      (dolist (spec versions)
        (let ((repo (car spec))
              (head (cdr spec)))
          (straight--set-head repo head)))
    (error "Could not read from %S" straight-versions-file)))

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

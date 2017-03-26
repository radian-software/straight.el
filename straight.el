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

;; There is not a one-to-one relationship between packages and
;; repositories. Normally, a repository will provide the Elisp files
;; for one and only one package, but this is not required. A single
;; repository can provide multiple packages, and there is no
;; requirement for a repository to provide any packages.
;;
;; A package is defined implicitly by its recipe. This idea is taken
;; from MELPA [1] and tweaked a bit. In straight.el, a package recipe
;; is a property list (check the Elisp manual to read more about
;; these) with a number of possible keys:
;;
;; :package - the name of the package as a string (two packages cannot
;;            have the same name)
;;
;; :local-repo - the name of the repository providing the package
;;
;; :files - a list specifying which files in the repository comprise
;;          the package; see MELPA [1] for more details on the syntax
;;          and default value
;;
;; :fetcher - a symbol identifying the method by which the repository
;;            can be retrieved from the Internet; one of `git',
;;            `github', `gitlab', `bitbucket', `bzr', `hg', `darcs',
;;            `fossil', `svn', `cvs', `wiki' (note that straight.el
;;            strongly prefers `git'; the other fetchers are only
;;            supported because I am using an adapted version [2] of
;;            the `package-build' code [3], which has this support)
;;
;; :url - the URL of the repository, for the `git', `bzr', `hg',
;;        `darcs', `fossil', `svn', and `cvs' fetchers
;;
;; :repo - the "username/repository", for the `github', `gitlab', and
;;         `bitbucket' fetchers
;;
;; :commit - the commit to check out, for the `git', `github', and
;;           `gitlab' fetchers
;;
;; :branch - the branch to check out, for the `git', `github', and
;;           `gitlab' fetchers
;;
;; :module - the module to check out, for the `cvs' fetcher
;;
;; [1]: https://github.com/melpa/melpa#recipe-format
;; [2]: https://github.com/raxod502/pbl
;; [3]: https://github.com/melpa/package-build

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

(defcustom straight-arrow
  (if (char-displayable-p ?→) " → " " -> ")
  "The string to use for an arrow in messages."
  :type 'string
  :group 'straight)

(defcustom straight-profiles
  '((nil . "default.el"))
  "Alist mapping package profile names to version lockfile names.
The profile names should be symbols, and the filenames should not
contain any directory components."
  :type '(alist :key-type symbol :value-type string))

(defcustom straight-current-profile
  nil
  "Symbol identifying the current package profile.
This symbol should have an entry in `straight-profiles'. If you
wish to take advantage of the multiple-profile system, you should
bind this variable to different symbols using `let' over
different parts of your init-file.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Displaying messages and warnings

(defmacro straight--with-progress (task &rest body)
  "Displaying TASK as a progress indicator, eval and return BODY.
Display \"TASK...\", eval BODY, display \"TASK...done\", and
return the result of evaluating BODY. If TASK is nil, no messages
are displayed. TASK can also be a cons, whose car and cdr are
used as the TASK for the beginning and end messages
respectively. (Either the car or cdr, or both, can be nil.) See
also `straight--progress-begin' and `straight--progress-end'."
  (declare (indent 1))
  (let ((task-sym (make-symbol "gensym--task"))
        (task-car-sym (make-symbol "gensym--task-car"))
        (task-cdr-sym (make-symbol "gensym--task-cdr")))
    `(let* ((,task-sym ,task)
            (,task-car-sym (if (listp ,task-sym)
                               (car ,task-sym)
                             ,task-sym))
            (,task-cdr-sym (if (listp ,task-sym)
                               (cdr ,task-sym)
                             ,task-sym)))
       (prog2
           (when ,task-car-sym
             (message "%s..." ,task-car-sym))
           (progn
             ,@body)
         (when ,task-cdr-sym
           (message "%s...done" ,task-cdr-sym))))))

(defun straight--progress-begin (message)
  "Display a MESSAGE indicating ongoing progress.
The MESSAGE is postpended with \"...\" and then passed to
`message'. See also `straight--with-progress' and
`straight--progress-end'."
  (message "%s..." message))

(defun straight--progress-end (message)
  "Display a MESSAGE indicating completed progress.
The MESSAGE is postpended with \"...done\" and then passed to
`message'. See also `"
  (message "%s...done" message))

(defun straight--warn (message &rest args)
  "Display a warning from `straight'.
The warning message is obtained by passing MESSAGE and ARGS to
`format'."
  (ignore
   (display-warning 'straight (apply #'format message args))))

(defvar straight--echo-area-dirty nil
  "Non-nil if a progress message has been wiped from the echo area.
This is used as an internal bookkeeping variable to determine if
a progress message has been bumped out of the echo area by
another message, and needs to be redisplayed.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Accessing and modifying data structures

(defmacro straight--with-plist (plist props &rest body)
  "Binding from PLIST the given PROPS, eval and return BODY.
PROPS is a list of symbols. Each one is converted to a keyword
and then its value is looked up in the PLIST and bound to the
symbol for the duration of BODY."
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

(defmacro straight--put (plist prop value)
  "Return a copy of the PLIST with key PROP mapped to VALUE."
  `(progn
     (setq ,plist (copy-sequence ,plist))
     (setq ,plist (plist-put ,plist ,prop ,value))))

(defun straight--insert (n key value table)
  "Associate index N in KEY with VALUE in hash table TABLE.
TABLE should be a hash whose values are lists. This function will
set the Nth entry of the list mapped to KEY in TABLE to VALUE. If
the list does not have an Nth entry, it will be padded with nils
so that it does, before the setting happens. The TABLE will be
modified and returned."
  (let ((list (gethash key table)))
    (if (>= n (length list))
        (puthash key
                 (append list
                         (make-list (- n (length list)) nil)
                         (list value))
                 table)
      (setcar (nthcdr n list) value))
    table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Constructing filesystem paths

(defun straight--dir (&rest segments)
  "Get a subdirectory of the straight.el directory.
The SEGMENTS are path segments which are concatenated with
slashes and postpended to the straight directory.

\(straight--dir \"build\" \"esup\")
=> \"~/.emacs.d/straight/build/esup/\""
  (expand-file-name
   (apply 'concat user-emacs-directory
          (mapcar (lambda (segment)
                    (concat segment "/"))
                  (cons "straight" segments)))))

(defun straight--file (&rest segments)
  "Get a file in the straight.el directory.
The SEGMENTS are path segments with are concatenated with slashes
and postpended to the straight directory.

\(straight--file \"build\" \"esup\" \"esup-autoloads.el\")
=> \"~/.emacs.d/straight/build/esup/esup-autoloads.el\""
  (expand-file-name
   (substring (apply 'straight--dir segments) 0 -1)))

(defun straight--autoload-file-name (package)
  "Get the bare filename of the autoload file for PACKAGE.
PACKAGE should be a string. The filename does not include the
directory component."
  (format "%s-autoloads.el" package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Calling external commands

(defun straight--check-call (command &rest args)
  "Call COMMAND with ARGS, returning non-nil if it succeeds.
If the COMMAND exits with a non-zero return code, return nil. If
the COMMAND does not exist, or if another error occurs, throw an
error."
  (= 0 (apply #'call-process command nil nil nil args)))

(defun straight--ensure-call (command &rest args)
  "Call COMMAND with ARGS, raising an error if it fails."
  (unless (apply #'straight--check-call command args)
    (error "Command failed: %s %s"
           command (string-join args " "))))

(defun straight--get-call (command &rest args)
  "Call COMMAND with ARGS, returning its stdout and stderr as a string.
Return a string with whitespace trimmed from both ends. If the
command fails, throw an error."
  (with-temp-buffer
    (unless (= 0 (apply #'call-process command
                        nil '(t t) nil args))
      (error "Command failed: %s %s"
             command (string-join args " ")))
    (string-trim (buffer-string))))

(defun straight--call-has-output-p (command &rest args)
  "Call COMMAND with ARGS, returning non-nil if it outputs something on stdout."
  (with-temp-buffer
    (apply #'straight--check-call command args)
    (> (buffer-size) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Fetching repositories

(defun straight--repository-is-available-p (recipe)
  "Determine if the repository for the RECIPE exists locally."
  (straight--with-plist recipe
      (local-repo)
    (file-exists-p (straight--dir "repos" local-repo))))

(defun straight--clone-repository (recipe &optional cause)
  "Clone the repository for the RECIPE, overwriting any existing copy.
CAUSE is a string indicating the reason this repository is being
cloned."
  (straight--with-plist recipe
      (package local-repo)
    (straight--with-progress
        (concat cause (when cause straight-arrow)
                (format "Cloning %s" local-repo)
                (unless (string= package local-repo)
                  (format " (for %s)" package)))
      (pbl-checkout
       local-repo recipe
       (straight--dir "repos" local-repo)))
    ;; We messed up the echo area.
    (setq straight--echo-area-dirty t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Recipe processing

(defvar gnu-elpa-url "git://git.savannah.gnu.org/emacs/elpa.git"
  "URL of the Git repository for the GNU ELPA package repository.")

;; These variables are used in the case that a bare package name is
;; passed to `straight--convert-recipe'. But their values are computed
;; by `straight--convert-recipe', so it's a bit of a chicken-and-egg
;; problem. Here we declare that their values will be assigned later,
;; so that the byte-compiler won't complain.
(defvar melpa-recipe)
(defvar gnu-elpa-recipe)
(defvar emacsmirror-recipe)

(defun straight--get-melpa-recipe (package &optional cause)
  "Look up a PACKAGE recipe in MELPA.
PACKAGE should be a symbol. If the package has a recipe listed in
MELPA, return it; otherwise return nil. If MELPA is not
available, clone it automatically before looking up the recipe.
CAUSE is a string explaining why MELPA might need to be cloned."
  (unless (straight--repository-is-available-p melpa-recipe)
    (straight--clone-repository melpa-recipe cause))
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

(defun straight--get-gnu-elpa-recipe (package &optional cause)
  "Look up a PACKAGE recipe in GNU ELPA.
PACKAGE should be a symbol. If the package is maintained in GNU
ELPA, a MELPA-style recipe is returned. Otherwise nil is
returned. If GNU ELPA is not available, clone it automatically
before looking up the recipe. CAUSE is a string explaining why
GNU ELPA might need to be cloned."
  (unless (straight--repository-is-available-p gnu-elpa-recipe)
    (straight--clone-repository gnu-elpa-recipe cause))
  (straight--with-plist gnu-elpa-recipe
      (local-repo)
    (when (file-exists-p (straight--dir
                          "repos" local-repo "packages"
                          (symbol-name package)))
      ;; All the packages in GNU ELPA are just subdirectories of the
      ;; same repository.
      `(,package :fetcher git
                 :url ,gnu-elpa-url
                 :files (,(format "packages/%s/*.el"
                                  (symbol-name package)))
                 :local-repo "elpa"))))

(defun straight--get-emacsmirror-recipe (package &optional cause)
  "Look up a PACKAGE recipe in Emacsmirror.
PACKAGE should be a symbol. If the package is available from
Emacsmirror, return a MELPA-style recipe; otherwise return nil.
If Emacsmirror is not available, clone it automatically before
looking up the recipe. CAUSE is a string explaining why
Emacsmirror might need to be cloned."
  (unless (straight--repository-is-available-p emacsmirror-recipe)
    (straight--clone-repository emacsmirror-recipe cause))
  (straight--with-plist emacsmirror-recipe
      (local-repo)
    (let ((default-directory (straight--dir "repos" local-repo)))
      ;; Try to get the URL for the submodule. If it doesn't exist,
      ;; return nil. This will work both for packages in the mirror
      ;; and packages in the attic.
      (when-let ((url (condition-case nil
                          (straight--get-call
                           "git" "config" "--file" ".gitmodules"
                           "--get" (format "submodule.%s.url"
                                           (symbol-name package)))
                        (error nil))))
        (and (not (string-empty-p url))
             ;; For the sake of elegance, we convert Github URLs to
             ;; use the `github' fetcher, if possible. At the time of
             ;; this writing, there are no Gitlab URLs (which makes
             ;; sense, since all the repositories should be hosted on
             ;; github.com/emacsmirror).
             (if (string-match
                  (concat "^git@github\\.com:\\([A-Za-z0-9_.-]+"
                          "/[A-Za-z0-9_.-]+\\)\\.git$")
                  url)
                 `(,package :fetcher github
                            :repo ,(match-string 1 url))
               `(,package :fetcher git
                          :url ,url)))))))

(defvar straight--repo-cache (make-hash-table :test #'equal)
  "Hash table listing known recipes by repository.
The keys are strings naming repositories, and the values are the
last known recipe that referenced the corresponding repository.
This is used for detecting conflicts (when multiple packages are
versioned in the same repository, but are specified with
incompatible recipes) and for silently adjusting recipes drawn
from recipe repositories so as to avoid conflicts.")

(defvar straight--fetch-keywords
  '(:fetcher :url :repo :commit :branch :module)
  "Keywords that affect how a repository is cloned.
If two recipes specify the same `:local-repo', but have different
values for any of these keywords, then they are incompatible.")

(defvar straight--build-keywords
  '(:local-repo :files)
  "Keywords that affect how a package is built locally.
If the values for any of these keywords change, then the package
needs to be rebuild.")

(defvar straight--keywords
  '(:local-repo :files :fetcher :url :repo :commit :branch :module)
  "Keywords that affect a package's definition.
If two recipes specify the same `:package', but have different
values for any of these keywords, then they are incompatible.")

(defun straight--lookup-recipe (package &optional sources cause)
  "Look up a PACKAGE recipe in one or more SOURCES.
PACKAGE should be a symbol, and SOURCES should be a list
containing one or more of `gnu-elpa', `melpa', and
`emacsmirror'. (If it is omitted, it defaults to allowing all
three sources.) Git-based MELPA recipes are preferred, then GNU
ELPA, then Emacsmirror, then non-Git MELPA recipes. If the recipe
is not found in any of the provided sources, raise an error.
CAUSE is a string indicating the reason recipe repositories might
need to be cloned."
  ;; We want to prefer Git, since that's the only VCS currently
  ;; supported. So we prefer MELPA recipes, but only if they are Git
  ;; repos, and then fall back to GNU ELPA and then Emacsmirror, and
  ;; as a last resort, non-Git MELPA recipes.
  (let* (;; If `sources' is omitted, allow all sources.
         (sources (or sources '(melpa gnu-elpa emacsmirror)))
         ;; Update the `cause' to explain why repositories might be
         ;; getting cloned.
         (cause (concat cause (when cause straight-arrow)
                        (format "Looking for %s recipe" package)))
         ;; Get the MELPA recipe first. We're keeping it around so
         ;; that if it's a non-Git-based recipe, but we can't find the
         ;; package anywhere else, then we can avoid needing to
         ;; recalculate the recipe.
         (melpa-recipe (and (member 'melpa sources)
                            (straight--get-melpa-recipe package cause))))
    ;; Git-based recipes in MELPA are ideal (since Git is the only VCS
    ;; that straight.el supports).
    (if (member (plist-get (cdr melpa-recipe) :fetcher)
                '(git github gitlab))
        melpa-recipe
      ;; Next most preferred is GNU ELPA.
      (or (and (member 'gnu-elpa sources)
               (straight--get-gnu-elpa-recipe package cause))
          ;; Emacsmirror comes after GNU ELPA because we prefer
          ;; "official" sources, so that it is easier to figure out
          ;; what upstream to submit changes against.
          (and (member 'emacsmirror sources)
               (straight--get-emacsmirror-recipe package cause))
          ;; This shouldn't be possible in normal cases, since
          ;; Emacsmirror ostensibly contains all packages that are in
          ;; MELPA. But you never know. It's better to return a
          ;; non-Git recipe than to error out entirely.
          melpa-recipe
          ;; Oh no!
          (error (concat "Could not find package %S "
                         "in MELPA, GNU ELPA, or "
                         "Emacsmirror")
                 package)))))

(defvar straight--recipe-cache (make-hash-table :test #'equal)
  "Hash table listing known recipes by package.
The keys are strings naming packages, and the values are the last
known recipe for that package. This is used for detecting
conflicting recipes for the same package; managing the build
cache and versions lockfile; and getting a list of all packages
in use.")

(defun straight--convert-recipe (melpa-style-recipe &optional cause)
  "Convert a MELPA-STYLE-RECIPE to a normalized straight.el recipe.
MELPA, GNU ELPA, and Emacsmirror may be cloned and searched for
recipes if the MELPA-STYLE-RECIPE is just a package name;
otherwise, the MELPA-STYLE-RECIPE should be a list and it is
modified slightly to conform to the internal straight.el recipe
format. CAUSE is a string indicating the reason recipe
repositories might need to be cloned."
  ;; Firstly, if the recipe is only provided as a package name, and
  ;; we've already converted it before, then we should just return the
  ;; previous result. This has nothing to do with efficiency; it's
  ;; actually to reduce conflicts. There are two common cases:
  ;;
  ;; 1. I'm overriding the standard recipe for a package with a custom
  ;;    recipe, and then loading a second package that requires the
  ;;    first one as a dependency. In that case, loading the second
  ;;    package will cause the first package to be loaded again,
  ;;    without any special information provided about its
  ;;    recipe (i.e. the recipe is supposed to be looked up by
  ;;    `straight--convert-recipe' in the recipe repositories). But
  ;;    that will cause a conflict, since a different recipe was
  ;;    already provided. This problem is solved by the following two
  ;;    lines of code. (Note, though, that there is still a conflict
  ;;    if you load the second package before its custom-recipe
  ;;    dependency, as should be expected.)
  ;;
  ;; 2. I'm loading two different features from the same package with
  ;;    `use-package', and providing a non-standard recipe for the
  ;;    package. For example, this comes up when you need to load both
  ;;    `tex-site' and `tex' from your fork of `auctex'. It's
  ;;    inconvenient to specify your custom recipe in both
  ;;    `use-package' declarations, but with the following two lines
  ;;    of code, you can specify your custom recipe in the first
  ;;    `use-package' declaration and then specify only `auctex' as
  ;;    the recipe in the second `use-package' declaration.
  (or (and (symbolp melpa-style-recipe)
           (gethash (symbol-name melpa-style-recipe) straight--recipe-cache))
      (let* (;; It's important to remember whether the recipe was
             ;; provided explicitly, or if it was just given as a
             ;; package name (meaning that the recipe needs to be
             ;; looked up in a recipe repository, i.e. MELPA, GNU
             ;; ELPA, or Emacsmirror). Why, you ask? It's so that we
             ;; can be a little more tolerant of conflicts in certain
             ;; cases -- see the comment below, before the block of
             ;; code that runs when `recipe-specified-p' is nil.
             (recipe-specified-p (listp melpa-style-recipe))
             ;; Now we normalize the provided recipe so that it is
             ;; still a MELPA-style recipe, but it is guaranteed to be
             ;; a list. This is the part where the recipe repositories
             ;; are consulted, if necessary.
             (full-melpa-style-recipe
              (if recipe-specified-p
                  melpa-style-recipe
                (straight--lookup-recipe melpa-style-recipe cause))))
        ;; MELPA-style recipe format is a list whose car is the
        ;; package name as a symbol, and whose cdr is a plist.
        (cl-destructuring-bind (package . plist) full-melpa-style-recipe
          ;; Recipes taken from MELPA would not normally have
          ;; `:local-repo' specified. But if the recipe was specified
          ;; manually, then you can specify `:local-repo' to override
          ;; the default value (which is determined according to
          ;; several heuristics based on `:repo', `:url', and
          ;; `:package').
          (straight--with-plist plist
              (local-repo repo url)
            ;; The normalized recipe format will have the package name
            ;; as a string, not a symbol.
            (let ((package (symbol-name package)))
              ;; Note that you can't override `:package'. That would
              ;; just be silly.
              (straight--put plist :package package)
              ;; This `unless' allows overriding `:local-repo' in a
              ;; manual recipe specification.
              (unless local-repo
                (straight--put plist :local-repo
                               ;; If the `:repo' is provided, then
                               ;; it's probably what the user wants
                               ;; the local repository name to be.
                               (or (when repo
                                     ;; Trim off the username, leaving
                                     ;; just the repository name.
                                     (replace-regexp-in-string
                                      "^.+/" "" repo))
                                   ;; The following is a half-hearted
                                   ;; attempt to turn arbitrary URLs
                                   ;; into reasonable repository
                                   ;; names.
                                   (let ((regexp "^.*/\\(.+\\)\\.git$"))
                                     (when (and url (string-match regexp url))
                                       (match-string 1 url)))
                                   ;; If all else fails, we can just
                                   ;; use the name of the package.
                                   package)))
              ;; This code is here to deal with complications that can
              ;; arise with manual recipe specifications when multiple
              ;; packages are versioned in the same repository.
              ;;
              ;; Specifically, let's suppose packages `swiper' and
              ;; `ivy' are both versioned in repository "swiper", and
              ;; let's suppose that I load both of them in my
              ;; init-file (`ivy' first and then `swiper'). Now
              ;; suppose that I discover a bug in `ivy' and fix it in
              ;; my fork, so that (until my fix is merged) I need to
              ;; provide an explicit recipe in my init-file's call to
              ;; `straight-use-package' for `ivy', in order to use my
              ;; fork. That will cause a conflict, because the recipe
              ;; for `swiper' is automatically taken from MELPA, and
              ;; it does not point at my fork, but instead at the
              ;; official repository. To fix the problem, I would have
              ;; to specify my fork in the recipe for `swiper' (and
              ;; also `counsel', a third package versioned in the same
              ;; repository). That violates DRY and is a pain.
              ;;
              ;; Instead, this code makes it so that if a recipe has
              ;; been automatically retrieved from a recipe repository
              ;; (for example, MELPA, GNU ELPA, or Emacsmirror), and
              ;; the `:local-repo' specified in that recipe has
              ;; already been used for another package, then the
              ;; configuration for that repository will silently be
              ;; copied over, and everything should "just work".
              ;;
              ;; Note that this weird edge case is totally unrelated
              ;; to the weird edge cases discussed earlier (in the
              ;; first comment of this function), and has to be
              ;; handled in a totally different way. It's surprising
              ;; how complicated recipe specification turns out to be.
              (unless recipe-specified-p
                (straight--with-plist plist
                    (local-repo)
                  ;; Here we are checking to see if there is already a
                  ;; formula with the same `:local-repo'. This is one
                  ;; of the primary uses of `straight--repo-cache'.
                  (when-let (original-recipe (gethash local-repo
                                                      straight--repo-cache))
                    ;; Copy all the potentially relevant keywords from
                    ;; the old recipe into the current one.
                    (dolist (keyword straight--fetch-keywords)
                      (when-let ((value (plist-get original-recipe keyword)))
                        (straight--put plist keyword value))))))
              ;; Return the newly normalized recipe.
              plist))))))

;; Now we can use the newly defined `straight--convert-recipe' to
;; define the recipes for the recipe repositories. This sounds like
;; infinite recursion, but it isn't -- remember that the recipe
;; repositories are only consulted by `straight--convert-recipe' in
;; the case that the MELPA-style recipe is provided as just a package
;; name (instead of a list). Note that there's no reason to provide a
;; `cause', since no messages should be printed.

(setq melpa-recipe (straight--convert-recipe
                    '(melpa :fetcher github
                            :repo "melpa/melpa")))

(setq gnu-elpa-recipe (straight--convert-recipe
                       `(elpa :fetcher git
                              :url ,gnu-elpa-url)))

(setq emacsmirror-recipe (straight--convert-recipe
                          ;; Note that `:nonrecursive' is an
                          ;; undocumented keyword only present in my
                          ;; fork of `package-build' (i.e. `pbl'),
                          ;; which prevents Git from cloning
                          ;; recursively. (Cloning `epkgs' recursively
                          ;; causes you to download all 6,000 or so
                          ;; known Emacs packages as submodules.)
                          `(epkgs :fetcher github
                                  :repo "emacsmirror/epkgs"
                                  :nonrecursive t)))

(defun straight--register-recipe (recipe)
  "Make the various caches aware of RECIPE.
RECIPE should be a straight.el-style recipe plist."
  (straight--with-plist recipe
      (package local-repo)
    ;; Step 1 is to check if the given recipe conflicts with an
    ;; existing recipe for a *different* package with the *same*
    ;; repository.
    (when-let ((existing-recipe (gethash local-repo straight--repo-cache)))
      ;; Only the `straight--fetch-keywords' are relevant for this,
      ;; not the full `straight--keywords' list.
      (cl-dolist (keyword straight--fetch-keywords)
        (unless (equal (plist-get recipe keyword)
                       (plist-get existing-recipe keyword))
          ;; We're using a warning rather than an error here, because
          ;; it's very frustrating if your package manager simply
          ;; refuses to install a package for no good reason. Note
          ;; that since we update `straight--repo-cache' and
          ;; `straight--recipe-cache' at the end of this method, this
          ;; warning will only be displayed once per recipe
          ;; modification.
          (straight--warn (concat "Packages %S and %S have incompatible "
                                  "recipes (%S cannot be both %S and %S)")
                          (plist-get existing-recipe :package)
                          package
                          keyword
                          (plist-get existing-recipe keyword)
                          (plist-get recipe keyword))
          (cl-return))))
    ;; Step 2 is to check if the given recipe conflicts with an
    ;; existing recipe for the *same* package.
    (when-let ((existing-recipe (gethash package straight--recipe-cache)))
      (cl-dolist (keyword straight--keywords)
        (unless (equal (plist-get recipe keyword)
                       (plist-get existing-recipe keyword))
          ;; Same reasoning as with the previous warning.
          (straight--warn "Package %S has two incompatible recipes "
                          "(%S cannot be both %S and %S)"
                          package
                          keyword
                          (plist-get existing-recipe keyword)
                          (plist-get recipe keyword))
          (cl-return))))
    ;; Step 3, now that we've signaled any necessary warnings, is to
    ;; actually update the caches. Just FYI, `straight--build-cache'
    ;; is updated later (namely, at build time -- which may be quite a
    ;; while later due to deferred installation via the
    ;; `only-if-installed' argument to `straight-use-package').
    (puthash package recipe straight--recipe-cache)
    (puthash local-repo recipe straight--repo-cache)
    (cl-pushnew straight-current-profile
                (gethash package straight--profile-cache)
                :test #'eq) ; profiles are symbols
    ;; If we've registered a new package, then we no longer know that
    ;; the set of registered packages actually corresponds to the
    ;; packages requested in the init-file. (For instance, this could
    ;; be an interactive call.)
    (setq straight--profile-cache-valid nil)))

(defun straight--map-repos (func)
  "Call FUNC for each local repository referenced in the known recipes.
FUNC is passed one argument, the straight.el-style recipe plist.
It is called once for every local repository (i.e. each distinct
value of `:local-repo'). This means that if multiple packages are
versioned in the same local repository, then all but one of them
will be omitted."
  ;; Remember that `straight--repo-cache' only has the most recent
  ;; recipe that specified each `:local-repo'.
  (dolist (recipe (hash-table-values straight--repo-cache))
    (funcall func recipe)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Managing repositories

(defun straight--version-controlled-p ()
  "Check if the current directory is the root of a Git repository."
  (file-exists-p ".git"))

(defun straight--fetch-remotes ()
  "Fetch history from all remotes of the current Git repository.
Raise an error if the command fails."
  (straight--ensure-call "git" "fetch" "--all" "--tags"
                         "--recurse-submodules"))

(defun straight--get-branch ()
  "Return the branch of the current Git repository.
If the HEAD is detached, return nil."
  (when-let ((symbolic-ref (condition-case nil
                               (straight--get-call
                                "git" "symbolic-ref" "HEAD")
                             (error nil))))
    (when (string-match "^refs/heads/\\(.+\\)" symbolic-ref)
      (match-string 1 symbolic-ref))))

(defun straight--get-upstream (branch)
  "Given a BRANCH, return its upstream (e.g. \"origin/master\").
If the branch has no upstream, return nil."
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
  "Return non-nil if ref ANCESTOR is an ancestor of ref DESCENDANT."
  (straight--check-call "git" "merge-base" "--is-ancestor"
                        ancestor descendant))

(defun straight--merge-with-upstream (upstream)
  "Merge the current branch with an UPSTREAM ref.
If the command fails, raise an error."
  (straight--ensure-call "git" "merge" upstream))

(defun straight--update-package (recipe)
  "Attempt to update the package specified by the RECIPE.
RECIPE should be a straight.el-style recipe plist. If the update
cannot be done, signal a warning."
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
  "Return the commit SHA of HEAD in the given LOCAL-REPO.
If the repository is not version-controlled with Git, return
nil."
  (let ((default-directory (straight--dir "repos" local-repo)))
    (when (straight--version-controlled-p)
      (straight--get-call "git" "rev-parse" "HEAD"))))

(defun straight--validate-head-is-reachable (local-repo head remote-urls)
  "Return non-nil if the LOCAL-REPO has a reachable HEAD.
This means that the commit SHA given by HEAD is reachable by some
commit available from at least one remote with a URL in the list
REMOTE-URLS."
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
  "In LOCAL-REPO, attempt to set HEAD to the given commit SHA.
If this cannot be done, signal a warning."
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
;;;; Managing package profiles

(defvar straight--profile-cache (make-hash-table :test #'equal)
  "Hash table mapping packages to lists of profiles.
The keys are strings naming packages, and the values are lists of
symbols identifying package profiles. These symbols are the
values that you bind `straight-current-profile' to, and they
should each have an entry in `straight-profiles'.")

(defvar straight--profile-cache-valid nil
  "Non-nil if `straight--profile-cache' accurately reflects the init-file.
The function `straight-save-versions' will be reluctant to create
a version lockfile if this variable is nil. It is set to non-nil
by the function `straight-declare-init-succeeded', and is set
back to nil when the straight.el bootstrap is run or
`straight-use-package' is invoked.")

(defun straight--reset-caches ()
  "Reset caches other than the build cache and success cache..
This means `straight--recipe-cache', `straight--repo-cache', and
`straight--profile-cache'. (We don't ever want to reset the build
cache since it is a totally separate system from the caches
employed by `straight--convert-recipe', and we don't ever want to
reset the success cache since that would mean the user would
receive a duplicate message if they called `straight-use-package'
interactively, reloaded their init-file, and then called
`straight-use-package' on the same package again.)"
  (setq straight--recipe-cache (make-hash-table :test #'equal))
  (setq straight--repo-cache (make-hash-table :test #'equal))
  (setq straight--profile-cache (make-hash-table :test #'equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Figuring out whether packages need to be rebuilt

(defvar straight--build-cache nil
  "Hash table keeping track of information about built packages, or nil.
The keys are strings naming packages, and the values are lists of
length three. The first entry is a timestamp identifying the last
time the package was successfully built; the second entry is a
list of the dependencies of the package, as strings; and the
third entry is the straight.el-normalized recipe plist for the
package. This information is used to determine whether or not a
package needs to be rebuilt.

The value of this variable is persisted in the file
build-cache.el.")

(defun straight--load-build-cache ()
  "Load the build cache from build-cache.el into `straight--build-cache'.
If build-cache.el does not both exist and contain a valid Elisp
form, then `straight--build-cache' is set to an empty hash table."
  (setq straight--build-cache
        (with-temp-buffer
          (ignore-errors
            ;; Using `insert-file-contents-literally' avoids
            ;; `find-file-hook', etc.
            (insert-file-contents-literally
             (straight--file "build-cache.el")))
          (or (ignore-errors
                (read (current-buffer)))
              ;; The keys are package names as *strings*.
              (make-hash-table :test #'equal)))))

(defun straight--save-build-cache ()
  "Write the build cache from `straight--build-cache' into build-cache.el."
  (with-temp-file (straight--file "build-cache.el")
    (pp straight--build-cache (current-buffer))))

(defvar straight--finalization-guaranteed nil
  "Non-nil if `straight-declare-init-finished' is guaranteed to be called.
This variable is part of the system that works to reduce
unnecessary saving and loading of the build cache. It is set to
non-nil when `straight-declare-init-finished' is called for the
first time, since the API of `straight-declare-init-finished'
requires the user to guarantee that if the function is called
once, then it will be called every time the user's init-file is
loaded (either at Emacs initialization or later, during a reload
of the init-file).")

(defvar straight--reinit-in-progress nil
  "Non-nil if init-file reloading is in progress.
This variable will be set to non-nil in bootstrap.el if
`straight--finalization-guaranteed' is non-nil. (If finalization
is not guaranteed, then reloading the init-file does not count,
since there is no way to reduce the saving and loading of the
build cache while still guaranteeing that the build cache will
actually get saved after init-file loading is complete.) It will
be set back to nil by `straight-declare-init-finished'.")

(defvar straight--after-reinit-hook nil
  "Normal hook run by `straight-declare-init-finished'.
It is run only on a reload of the init-file (not on Emacs
initialization), and only if there has been at least one
invocation of `straight-use-package' since the last time the
init-file was loaded.

The usual contents of this hook are either
`straight--save-build-cache', or nothing.")

(defun straight--maybe-load-build-cache ()
  "Invoke `straight--load-build-cache' if necessary.
During init, loading will take place only the first time this
method is called. After init, loading will take place every time.
The exception to this is if the user calls
`straight-declare-init-finished' (which see) in their init-file,
which allows for loading to again only take place once when the
user reloads their init-file."
  (cond
   ;; During initialization, we use `after-init-hook'.
   ((not after-init-time)
    (unless (member #'straight--save-build-cache after-init-hook)
      (straight--load-build-cache))
    (add-hook 'after-init-hook #'straight--save-build-cache))
   ;; After initialization, we use `straight--after-reinit-hook'.
   (straight--reinit-in-progress
    (unless (member #'straight--save-build-cache straight--after-reinit-hook)
      (straight--load-build-cache))
    (add-hook 'straight--after-reinit-hook #'straight--save-build-cache))
   ;; Otherwise, we just load the cache every time. (This is slower,
   ;; but totally reliable.)
   (t (straight--load-build-cache))))

(defun straight--maybe-save-build-cache ()
  "Invoke `straight--save-build-cache' if necessary.
During init, saving will not take place until `after-init-hook'.
After init, saving will take place every time. The exception to
this is if the user calls `straight-declare-init-finished' (which
see) in their init-file, which allows for saving to again only
take place once (when `straight-declare-init-finished' is
called)."
  (unless straight--reinit-in-progress
    (straight--save-build-cache)))

(defvar straight--cached-packages-might-be-modified-p :unknown
  "Non-nil if any of the packages in the build cache might be modified.
This variable is used as a cache to memoize the function
`straight--cached-packages-might-be-modified-p'. (Its value is
`:unknown' if that function has not yet been called to get the
value to cache.)")

(defun straight--cached-packages-might-be-modified-p ()
  "Check whether any of the packages in the build cache might be modified.
This is done by using find(1) to recursively check the mtimes of
all packages in the build cache against the last build time
recorded in the build cache. This function is memoized using the
variable `straight--cached-packages-might-be-modified-p', but the
memoized value is considered stale if Emacs initialization has
been completed."
  (cond
   ;; If the value has not been calculated yet, then we can calculate
   ;; and return it whether or not it's during init.
   ((eq straight--cached-packages-might-be-modified-p :unknown)
    (setq straight--cached-packages-might-be-modified-p
          (let ((repos nil) ; prevent double-checking repositories
                (args nil)) ; find(1) argument list
            (maphash
             (lambda (package build-info)
               (cl-destructuring-bind (mtime dependencies recipe) build-info
                 (straight--with-plist recipe
                     (local-repo)
                   ;; It might be faster to use a hash table to keep
                   ;; track of the known repos. (We can't use
                   ;; `straight--repo-cache' since it's not
                   ;; initialized yet; see the comment before the
                   ;; second argument to `maphash'.)
                   (unless (member local-repo repos)
                     (push local-repo repos)
                     ;; The basic idea of the find(1) command here is
                     ;; that it is composed of a series of disjunctive
                     ;; clauses, one for each repository. The first
                     ;; clause matches anything named ".git" at a
                     ;; depth of two, so that the Git directories are
                     ;; ignored. Then each subsequent clause matches
                     ;; and prints anything in a particular repository
                     ;; that has an mtime greater than the last build
                     ;; time for that repository. Just in case you're
                     ;; wondering why this is done as a single find(1)
                     ;; command, it's because it's substantially
                     ;; faster. Of course, this command is a total
                     ;; shot in the dark, since the list of packages
                     ;; required in the init-file could have
                     ;; completely changed since the last time the
                     ;; build cache was written, but if that *hasn't*
                     ;; happened then we're in luck and this
                     ;; optimization speeds up init quite a lot.
                     ;;
                     ;; Just FYI, this find(1) command is compatible
                     ;; with both GNU and BSD find.
                     (setq args (append (list "-o"
                                              "-path"
                                              (format "./%s/*" local-repo)
                                              "-newermt"
                                              mtime
                                              "-print")
                                        args))))))
             ;; The variables `straight--recipe-cache' and
             ;; `straight--repo-cache' have not yet been
             ;; populated (this code will be run at the very first
             ;; invocation of `straight-use-package'), so we must use
             ;; the build cache rather than either of those two.
             straight--build-cache)
            ;; The preamble to the find(1) command, which comes before
            ;; the repository-specific subparts (see above).
            (setq args (append (list "." "-depth" "2" "-name" ".git" "-prune")
                               args))
            (with-temp-buffer
              (let ((default-directory (straight--dir "repos")))
                ;; We're interleaving both stdout and stderr. The
                ;; command shouldn't print anything if everything is
                ;; as expected (and nothing has changed since the last
                ;; build of each package).
                (apply 'call-process "find" nil '(t t) nil args)
                ;; If find(1) prints anything then we are screwed and
                ;; have to check each package individually later on.
                ;; That is,
                ;; `straight--cached-packages-might-be-modified-p'
                ;; will be non-nil.
                (> (buffer-size) 0))))))
   ;; If the value has already been calculated, but we've finished
   ;; init, then we can't trust that value any more.
   (after-init-time t)
   ;; But if init is still in progress, then we can just use the
   ;; cached value.
   (t straight--cached-packages-might-be-modified-p)))

(defun straight--package-might-be-modified-p (recipe)
  "Check whether the package for the given RECIPE might be modified.
This is done by using find(1) to recursively check the mtimes of
all files in the package's local repository."
  (straight--with-plist recipe
      (package local-repo)
    (let* (;; `build-info' is a list of length three containing the
           ;; timestamp of the last build, the list of dependencies,
           ;; and the recipe plist, in that order.
           (build-info (gethash package straight--build-cache))
           (last-mtime (nth 0 build-info))
           (last-recipe (nth 2 build-info)))
      (or (null build-info)
          ;; Rebuild if relevant parts of the recipe have changed.
          (cl-dolist (keyword straight--build-keywords nil)
            (unless (equal (plist-get recipe keyword)
                           (plist-get last-recipe keyword))
              (cl-return t)))
          (and (straight--cached-packages-might-be-modified-p)
               ;; If at least one of the cached packages has
               ;; changed (we know by this point that the package
               ;; under consideration was present in the build cache),
               ;; then we still only need to rebuild it if it
               ;; specifically has changed.
               (or (not (stringp last-mtime)) ; shouldn't happen
                   (with-temp-buffer
                     (let ((default-directory
                             (straight--dir "repos" local-repo)))
                       (call-process
                        ;; This find(1) command ignores the .git
                        ;; directory, and prints the names of any
                        ;; files or directories with a newer mtime
                        ;; than the one specified.
                        "find" nil '(t t) nil
                        "." "-name" ".git" "-prune"
                        "-o" "-newermt" last-mtime "-print")
                       (> (buffer-size) 0)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Building packages

(defun straight--symlink-package (recipe)
  "Symlink the package for the given RECIPE into the build directory.
This deletes any existing files in the relevant subdirectory of
the build directory, creating a pristine set of symlinks."
  (straight--with-plist recipe
      (package local-repo files)
    ;; Remove the existing built package, if necessary.
    (let ((dir (straight--dir "build" package)))
      (when (file-exists-p dir)
        (delete-directory dir 'recursive)))
    ;; Make a new directory for the built package.
    (make-directory (straight--dir "build" package) 'parents)
    ;; We use `pbl' to process MELPA's DSL for file copying. (We're
    ;; doing the actual actions on the filesystem ourselves, though,
    ;; since MELPA uses copying rather than symlinking like we need.
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
  "Normalize a package.el-style list of DEPENDENCIES.
Each dependency is a list of length two containing a symbol
naming a package and a string naming the minimum version
required (see the Package-Requires header in a
package.el-compliant Elisp package). The return value is a list
of strings naming the packages that are mentioned in the
dependency list, excluding built-in packages (as determined by
package.el)."
  ;; We don't want package.el unless dependencies need to be
  ;; processed. Since we're loading it dynamically, we need to do
  ;; `eval-and-compile' to silence the byte-compiler.
  (eval-and-compile
    (require 'package))
  (let ((packages nil))
    (dolist (spec dependencies)
      (cl-destructuring-bind (package . version) spec
        (unless (package-built-in-p package)
          (push (symbol-name package) packages))))
    packages))

(defun straight--compute-dependencies (package)
  "Register the dependencies of PACKAGE in `straight--build-cache'.
PACKAGE should be a string naming a package. Note that this
function does *not* return the dependency list; see
`straight--get-dependencies' for that. (The reason these two
functions are separate is because dependencies are computed at
package build time, but they are retrieved later (when we are
activating autoloads, and may not have even built the package on
this run of straight.el)."
  (let ((dependencies
         ;; There are actually two ways of specifying a package in
         ;; Emacs. The first is to include a file called
         ;; <PACKAGE-NAME>-pkg.el which contains a data structure with
         ;; a bunch of information (including the dependency alist).
         ;; The second is to put the information as headers in the
         ;; preamble of the file <PACKAGE-NAME>.el. We account for
         ;; both of them here.
         (or (condition-case nil
                 (with-temp-buffer
                   (insert-file-contents-literally ; bypass `find-file-hook'
                    (straight--file
                     "build" package
                     (format "%s-pkg.el" package)))
                   (straight--process-dependencies
                    (eval (nth 4 (read (current-buffer))))))
               (error nil))
             (condition-case nil
                 (with-temp-buffer
                   (insert-file-contents-literally ; bypass `find-file-hook'
                    (straight--file
                     "build" package
                     (format "%s.el" package)))
                   ;; Who cares if the rest of the header is
                   ;; well-formed? Maybe package.el does, but all we
                   ;; really need is the dependency alist. If it's
                   ;; missing or malformed, we just assume the package
                   ;; has no dependencies.
                   (re-search-forward "^;; Package-Requires: ")
                   (straight--process-dependencies
                    (read (current-buffer))))
               (error nil)))))
    (straight--insert 1 package dependencies straight--build-cache)))

(defun straight--get-dependencies (package)
  "Get the dependencies of PACKAGE from `straight--build-cache'.
PACKAGE should be a string naming a package. This assumes that
they were previously registered in the build cache by
`straight--compute-dependencies'."
  (nth 1 (gethash package straight--build-cache)))

(defun straight--generate-package-autoloads (recipe)
  "Generate autoloads for the symlinked package specified by RECIPE.
RECIPE should be a straight.el-style plist. See
`straight--autoload-file-name'. Note that this function only
modifies the build folder, not the original repository."
  (straight--with-plist recipe
      (package)
    (let (;; The full path to the autoload file.
          (generated-autoload-file
           (straight--file
            "build" package
            (straight--autoload-file-name package)))
          ;; The following bindings are in
          ;; `package-generate-autoloads'. Presumably this is for a
          ;; good reason, so I just copied them here. It's a shame
          ;; that Emacs activates so many random features even when
          ;; you are accessing files programmatically.
          (noninteractive t)
          (backup-inhibited t)
          (version-control 'never)
          ;; Tell Emacs to shut up.
          (message-log-max nil) ; no *Messages*
          (inhibit-message t)) ; no echo area
      ;; Actually generate the autoload file.
      (update-directory-autoloads
       (straight--dir "build" package))
      ;; And for some reason Emacs leaves a newly created buffer lying
      ;; around. Let's kill it.
      (when-let ((buf (find-buffer-visiting generated-autoload-file)))
        (kill-buffer buf)))))

(defun straight--byte-compile-package (recipe)
  "Byte-compile autoloads for the symlinked package specified by RECIPE.
RECIPE should be a straight.el-tsyle plist. Note that this
function only modifies the build folder, not the original
repository."
  (straight--with-plist recipe
      (package)
    ;; These two `let' forms try very, very hard to make
    ;; byte-compilation an invisible process. Lots of packages have
    ;; byte-compile warnings; I don't need to know about them and
    ;; neither do straight.el users.
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
        ;; Note that there is in fact no `byte-compile-directory'
        ;; function.
        (byte-recompile-directory
         (straight--dir "build" package)
         0 'force)))))

(defun straight--finalize-build (recipe)
  "Update `straight--build-cache' to reflect a successful build of RECIPE.
RECIPE should be a straight.el-style plist. The build mtime and
recipe in `straight--build-cache' for the package are updated."
  (straight--with-plist recipe
      (package local-repo)
    (let (;; This time format is compatible with BSD and GNU find(1).
          ;; Which is why we're using it, of course.
          (mtime (format-time-string "%FT%T%z")))
      (straight--insert 0 package mtime straight--build-cache))
    (straight--insert 2 package recipe straight--build-cache)))

(defun straight--build-package (recipe &optional cause)
  "Build the package specified by the RECIPE.
This includes symlinking the package files into the build
directory, building dependencies, generating the autoload file,
byte-compiling, and updating the build cache. It is assumed that
the package repository has already been cloned.

RECIPE is a straight.el-style plist. CAUSE is a string indicating
the reason this package is being built."
  (straight--with-plist recipe
      (package)
    (let ((task (concat cause (when cause straight-arrow)
                        (format "Building %s" package))))
      (straight--with-progress task
        (straight--symlink-package recipe)
        (straight--compute-dependencies package)
        ;; Before we (possibly) build the dependencies, we need to set
        ;; this flag so that we know if our progress message will need
        ;; to be redisplayed afterwards (before autoload generation
        ;; and byte-compilation).
        (setq straight--echo-area-dirty nil)
        ;; Yes, we do the following logic twice. Once here and again
        ;; in `straight-use-package'. Why? We need to do it here
        ;; because the dependencies need to be available before this
        ;; package can be byte-compiled. But the normal case is that
        ;; packages are already going to be built, so this code path
        ;; will not be hit and therefore autoloads will not be
        ;; generated for the dependencies in that situation if we
        ;; don't do it again in `straight-use-package'.
        (when-let ((dependencies (straight--get-dependencies package)))
          (dolist (dependency dependencies)
            ;; The implicit meaning of the first argument to
            ;; `straight-use-package' here is that the default
            ;; recipes (taken from one of the recipe repositories) are
            ;; used for dependencies. (Well, maybe. See all the weird
            ;; edge cases and exceptions in
            ;; `straight--convert-recipe'.) Note that the second
            ;; argument is always nil. That is, even if the user was
            ;; supposed to be prompted about whether they wanted to
            ;; install this package, if they say yes then all the
            ;; dependencies are automatically installed (what else
            ;; could the user want?).
            (straight-use-package (intern dependency) nil task))
          ;; We might need to redisplay the progress message from
          ;; `straight--with-progress' up above.
          (when straight--echo-area-dirty
            (straight--progress-begin task)))
        (straight--generate-package-autoloads recipe)
        (straight--byte-compile-package recipe)
        ;; This won't get called if there is an error.
        (straight--finalize-build recipe))
      ;; We messed up the echo area.
      (setq straight--echo-area-dirty t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Loading packages

(defun straight--add-package-to-load-path (recipe)
  "Add the package specified by RECIPE to the `load-path'.
RECIPE is a straight.el-style plist. It is assumed that the
package has already been built."
  (straight--with-plist recipe
      (package)
    (add-to-list 'load-path (straight--dir "build" package))))

(defun straight--activate-package-autoloads (recipe)
  "Evaluate the autoloads for the package specified by RECIPE.
This means that the functions with autoload cookies in the
package are now autoloaded and calling them will `require' the
package. It is assumed that the package has already been built.

RECIPE is a straight.el-style plist."
  (straight--with-plist recipe
      (package)
    (load (straight--file
           "build" package (straight--autoload-file-name package))
          nil 'nomessage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Interactive helpers

(defun straight--select-package (message)
  "Use `completing-read' to select a package.
MESSAGE is displayed as the prompt; it should not end in punctuation
or whitespace."
  (completing-read
   (concat message ": ")
   (hash-table-keys straight--recipe-cache)
   (lambda (elt) t)
   'require-match))

(defun straight--map-repo-packages (func)
  "Call FUNC for each local repository referenced in the known recipes.
The function FUNC is passed one argument, the name (as a string)
of one of the packages using the local repository."
  (straight--map-repos
   (lambda (recipe)
     (straight--with-plist recipe
         (package)
       (funcall func package)))))

(defun straight--get-recipe-interactively (sources &optional action)
  "Use `completing-read' to select an available package.
SOURCES is a list containing one or more of `melpa', `gnu-elpa',
and `emacsmirror'. (If it is nil, then all three of the sources
are assumed to be present.) The relevant recipe repositories are
cloned if necessary first. If `action' is nil or omitted, return
the recipe. If ACTION is `insert', then insert it into the
current buffer. If ACTION is `copy', then insert it into the
kill ring. Interactively, copy it if a prefix argument is
provided, and insert it otherwise."
  (let ((sources (or sources '(melpa gnu-elpa emacsmirror))))
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
           ;; No need to provide a `cause' to
           ;; `straight--lookup-recipe'; it should not be printing any
           ;; messages.
           (recipe (straight--lookup-recipe package sources)))
      (pcase action
        ('insert (insert (format "%S" recipe)))
        ('copy (kill-new (format "%S" recipe))
               (message "Copied \"%S\" to kill ring" recipe))
        (_ recipe)))))

(defvar straight--success-cache (make-hash-table :test #'equal)
  "Hash table containing successfully built packages as keys.
The values are meaningless, and all non-nil.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; API

;;;###autoload
(defun straight-declare-init-finished ()
  "Declare that init-file loading has finished.
If you call this function, then you are required to ensure that
it is called *every time* your init-file is loaded, even if there
is an error. Calling this function is not mandatory, but it
allows the use of an optimization that significantly improves the
performance of reloading an init-file with many
`straight-use-package' forms in it."
  (setq straight--reinit-in-progress nil)
  (setq straight--finalization-guaranteed t)
  (run-hooks 'straight--after-reinit-hook))

;;;###autoload
(defun straight-declare-init-succeeded ()
  "Declare that init-file loading has succeeded.
This function should not be called unless init (or re-init) has
finished with no errors. Calling this function is not mandatory,
but it is strongly recommended if you want to use a version
lockfile, since it allows straight.el to ensure that only
packages actually in your init-file are written to the lockfile.
It also improves performance by allowing the build cache to be
pruned."
  (setq straight--profile-cache-valid t)
  ;; We can safely prune the build cache if init succeeded.
  (dolist (package (hash-table-keys straight--build-cache))
    (unless (gethash package straight--recipe-cache)
      (remhash package straight--build-cache))))

;;;###autoload
(defun straight-get-recipe (&optional action)
  "Interactively select a recipe from one of the recipe repositories.
All three recipe repositories will first be cloned. If a prefix
argument is provided, copy the recipe to the kill ring;
otherwise, insert it into the current buffer. From Lisp code,
copying is achieved by passing ACTION as `copy'; insertion is
achieved by passing ACTION as `insert'; neither are done if
ACTION is nil or omitted."
  (interactive (list (if current-prefix-arg
                         'copy
                       'insert)))
  (straight--get-recipe-interactively nil action))

;;;###autoload
(defun straight-get-melpa-recipe (&optional action)
  "Interactively select a MELPA recipe. See `straight-get-recipe'.
ACTION can be nil, `copy', or `insert'."
  (interactive (list (if current-prefix-arg
                         'copy
                       'insert)))
  (straight--get-recipe-interactively '(melpa) action))

;;;###autoload
(defun straight-get-gnu-elpa-recipe (&optional action)
  "Interactively select a GNU ELPA recipe. See `straight-get-recipe'.
ACTION can be nil, `copy', or `insert'."
  (interactive (list (if current-prefix-arg
                         'copy
                       'insert)))
  (straight--get-recipe-interactively '(gnu-elpa) action))

;;;###autoload
(defun straight-get-emacsmirror-recipe (&optional action)
  "Interactively select an Emacsmirror recipe. See `straight-get-recipe'.
ACTION can be nil, `copy', or `insert'."
  (interactive (list (if current-prefix-arg
                         'copy
                       'insert)))
  (straight--get-recipe-interactively '(emacsmirror) action))

;;;###autoload
(defun straight-use-package
    (melpa-style-recipe &optional only-if-installed cause interactive)
  "Install, build, and activate a package and its dependencies.
This is the main entry point to the functionality of straight.el.

MELPA-STYLE-RECIPE is either a symbol naming a package, or a list
whose car is a symbol naming a package and whose cdr is a
property list containing e.g. `:fetcher', `:repo', `:local-repo',
`:files', etc. By default, if the local repository for a package
is not available, it is cloned automatically. This behavior can
be suppressed by passing a non-nil value for ONLY-IF-INSTALLED.
If ONLY-IF-INSTALLED is `prompt', `y-or-n-p' is used to determine
its value. CAUSE is a string explaining the reason why
`straight-use-package' has been called. It is for internal use
only, and is used to construct progress messages. INTERACTIVE is
non-nil if the function has been called interactively. It is for
internal use only, and is used to determine whether to show a
hint about how to install the package permanently.

Return non-nil if package was actually installed, and nil
otherwise (this can only happen if ONLY-IF-INSTALLED is
non-nil)."
  (interactive (list (straight--get-recipe-interactively nil)
                     nil nil 'interactive))
  (let ((recipe (straight--convert-recipe melpa-style-recipe cause)))
    (straight--with-plist recipe
        (package)
      (let (;; Check if the package has been successfully built. If
            ;; not, and this is an interactive call, we'll want to
            ;; display a helpful hint message (see below).
            (already-registered
             (gethash package straight--success-cache)))
        ;; We need to register the recipe before building the package,
        ;; since the ability of `straight--convert-recipe' to deal
        ;; properly with dependencies versioned in the same repository
        ;; of their parent package will break unless the caches are
        ;; updated before we recur to the dependencies.
        (straight--register-recipe recipe)
        (let ((available
               (straight--repository-is-available-p recipe)))
          ;; If the condition in this `unless' evaluates to non-nil,
          ;; the package was not installed. Return nil.
          (unless (and (not available)
                       only-if-installed
                       (not (and (eq only-if-installed 'prompt)
                                 (y-or-n-p
                                  (format "Install package %S? "
                                          package)))))
            (unless available
              (straight--clone-repository recipe cause))
            (straight--maybe-load-build-cache)
            (when (straight--package-might-be-modified-p recipe)
              (straight--build-package recipe interactive))
            (straight--maybe-save-build-cache)
            ;; Here we are not actually trying to build the
            ;; dependencies, but activate their autoloads. (See the
            ;; comment in `straight--build-package' about this code.)
            (dolist (dependency (straight--get-dependencies package))
              ;; There are three interesting things here. Firstly, the
              ;; recipe used is just the name of the dependency. This
              ;; causes the default recipe to be looked up, unless one
              ;; of the special cases in `straight--convert-recipe'
              ;; pops up. Secondly, the value of `only-if-installed'
              ;; is always nil. If the user has agreed to install a
              ;; package, we assume that they also want to install all
              ;; of its dependencies without further prompts. Finally,
              ;; we don't bother to update `cause', since we're not
              ;; expecting any messages to be displayed here (all of
              ;; the dependencies should have already been cloned [if
              ;; necessary] and built back by
              ;; `straight--build-package').
              (straight-use-package (intern dependency) nil cause))
            ;; Only make the package available after all of its
            ;; dependencies are OK.
            (straight--add-package-to-load-path recipe)
            (straight--activate-package-autoloads recipe)
            ;; In interactive use, tell the user how to install
            ;; packages permanently.
            (when (and interactive (not already-registered))
              (message
               (concat "If you want to keep %s, put "
                       "(straight-use-package %s%S) "
                       "in your init-file.")
               package "'" (intern package)))
            ;; The package was installed successfully.
            (puthash package t straight--success-cache)
            t))))))

;;;###autoload
(defun straight-update-package (package)
  "Try to update a PACKAGE.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'."
  (interactive (list (straight--select-package "Update package")))
  (straight--with-progress (format "Updating package %S" package)
    (straight--update-package (gethash package straight--recipe-cache))))

;;;###autoload
(defun straight-update-all ()
  "Try to update all packages."
  (interactive)
  (straight--map-repo-packages #'straight-update-package))

;;;###autoload
(defun straight-validate-package (package &optional nomsg)
  "Check if a PACKAGE's configuration is reproducible.
This means that it is versioned with Git and its HEAD is
reachable from at least one branch in one of the remotes with a
URL consistent with the one specified in the recipe.

PACKAGE is a string naming a package that has been activated in
the current Emacs session. Return non-nil if the package is
valid, and nil otherwise. If the package is not valid, display a
warning. If it is valid, display a message, unless NOMSG is
non-nil. Interactively, select a package using `completing-read'
and set NOMSG to nil."
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
                            ;; Allow both HTTPS and SSH protocols;
                            ;; they function identically and both are
                            ;; seen in the wild (e.g. `pbl' uses HTTPS
                            ;; by default but the hub(1) command line
                            ;; utility uses SSH by default when
                            ;; creating forks).
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
  "Validate all packages. See `straight-validate-package'.
Any invalid packages result in warnings. A summary of the numbers
of valid and invalid packages is displayed as a message, unless
NOMSG is non-nil (this is not the case in interactive usage)."
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
(defun straight-freeze-versions (&optional force)
  "Write version lockfiles for currently activated packages.
If any packages have non-reproducible configurations (see
`straight-validate-package'), refuse to write the lockfile. If
the package management system has been used since the last time
the init-file was reloaded (or `straight-declare-init-succeeded'
is not called in the init-file), refuse to write the lockfile. If
FORCE is non-nil (interactively, if a prefix argument is
provided), skip these checks and write the lockfile anyway.

Multiple lockfiles may be written (one for each profile),
according to the value of `straight-profiles'."
  (interactive "P")
  (when (or force
            (and straight--profile-cache-valid
                 (ignore
                  (message (concat "Caches are outdated, aborting "
                                   (if straight--finalization-guaranteed
                                       "(please reload your init-file)"
                                     "(please restart Emacs)")))))
            (and (straight-validate-all 'nomsg)
                 (ignore
                  (message
                   "Not all packages have reachable HEADS, aborting"))))
    (maphash
     (lambda (profile versions-lockfile)
       (let ((versions-alist nil)
             (lockfile-directory (straight--dir "versions"))
             (lockfile-path (straight--file "versions" versions-lockfile)))
         (straight--map-repos
          (lambda (recipe)
            (straight--with-plist recipe
                (package local-repo)
              (when (memq profile (gethash package straight--profile-cache))
                (push (cons local-repo
                            (straight--get-head
                             local-repo))
                      versions-alist)))))
         (setq versions-alist
               (cl-sort versions-alist #'string-lessp :key #'car))
         (make-directory lockfile-directory 'parents)
         (with-temp-file lockfile-path
           (pp versions-alist (current-buffer)))
         (message "Wrote %s" lockfile-path)))
     straight-profiles)))

;;;###autoload
(defun straight-thaw-versions ()
  "Read version lockfiles and restore package versions to those listed."
  (interactive)
  (maphash
   (lambda (profile versions-lockfile)
     (let ((lockfile-path (straight--file "versions" versions-lockfile)))
       (if-let ((versions-alist (with-temp-buffer
                                  (insert-file-contents-literally
                                   lockfile-path)
                                  (ignore-errors
                                    (read (current-buffer))))))
           (dolist (spec versions-alist)
             (let ((repo (car spec))
                   (head (cdr spec)))
               (straight--set-head repo head)))
         (straight--warn "Could not read from %S" lockfile-path))))
   straight-profiles))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mess with other packages

;; Prevent package.el from inserting a call to `package-initialize' in
;; the init-file.
(setq package-enable-at-startup nil)

(with-eval-after-load 'use-package
  ;; Declare variables and functions from `use-package' to the
  ;; byte-compiler.
  (defvar use-package-keywords)
  (defvar use-package-defaults)
  (defvar use-package-ensure-function)
  (defvar use-package-pre-ensure-function)
  (declare-function use-package-only-one "use-package")
  (declare-function use-package-process-keywords "use-package")
  (declare-function use-package-as-symbol "use-package")
  ;; Register aliases for :ensure. Aliases later in the list will
  ;; override those earlier. (But there is no legitimate reason to use
  ;; more than one in a `use-package' declaration, at least in sane
  ;; situations.) The reason we also handle `:ensure' is because the
  ;; default value of `use-package-normalize/:ensure' is not flexible
  ;; enough to handle recipes like we need it to.
  (dolist (keyword '(:quelpa :recipe :ensure))
    ;; Insert the keyword just before `:ensure'.
    (unless (member keyword use-package-keywords)
      (setq use-package-keywords
            (let* ((pos (cl-position :ensure use-package-keywords))
                   (head (cl-subseq use-package-keywords 0 pos))
                   (tail (cl-subseq use-package-keywords pos)))
              (append head (list keyword) tail))))
    ;; Define the normalizer for the keyword.
    (eval
     `(defun ,(intern (format "use-package-normalize/%S" keyword))
          (name-symbol keyword args)
        (use-package-only-one (symbol-name keyword) args
          (lambda (label arg)
            (if (keywordp (car-safe arg))
                (cons name-symbol arg)
              arg)))))
    ;; Define the handler. We don't need to do this for `:ensure'.
    (unless (eq keyword :ensure)
      (eval
       `(defun ,(intern (format "use-package-handler/%S" keyword))
            (name keyword recipe rest state)
          (use-package-process-keywords
            name rest (plist-put state :recipe recipe))))))
  ;; Make it so that `:ensure' uses `straight-use-package' instead of
  ;; `package-install'.
  (defun straight--use-package-ensure-function
      (name ensure state context &optional only-if-installed)
    (when ensure
      (let ((recipe (or (and (not (eq ensure t)) ensure)
                        (plist-get state :recipe)
                        name)))
        (straight-use-package
         recipe (or only-if-installed
                    (unless (member context '(:byte-compile :ensure :config))
                      'prompt))))))
  (defun straight--use-package-pre-ensure-function
      (name ensure state)
    (straight--use-package-ensure-function
     name ensure state nil 'ignore))
  ;; The last two function definitions are not at the top level, so
  ;; the byte-compiler doesn't know about them unless we explicitly
  ;; use `declare-function'.
  (declare-function straight--use-package-ensure-function "straight")
  (declare-function straight--use-package-pre-ensure-function "straight")
  ;; Set the package management functions
  (setq use-package-ensure-function
        #'straight--use-package-ensure-function)
  (setq use-package-pre-ensure-function
        #'straight--use-package-pre-ensure-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Closing remarks

(provide 'straight)

;;; straight.el ends here

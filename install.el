;;; install.el --- Install straight.el from scratch -*- lexical-binding: t -*-

;; Caution: this file contains copious amounts of magic. You have been
;; warned.

;; For the sake of sanity, here is the issue. We want to install
;; straight.el, but to do that we need to figure out what repository
;; to install it from. That requires parsing the recipe that the user
;; has provided, if any. But not only is that logic a part of
;; straight.el itself, but the logic changes depending on the version
;; of straight.el. It would seem to be impossible to get straight.el
;; to correctly install any version of itself, without hardcoding the
;; repository. Not so!
;;
;; We start out by making sure that a recipe version identifier is
;; saved in the version lockfiles whenever they are saved. We put the
;; version identifier in the lockfiles because if the lockfiles are
;; absent, then we are just installing the latest version of
;; straight.el anyway, so there is no need for the version identifier
;; in that case.
;;
;; Then we look for lockfiles, and determine the version identifier.
;; This is automatically translated into a branch name for
;; straight.el. The idea is that while there are lots of revisions of
;; straight.el, only a few actually change the interpretation of
;; recipe fetch keywords. And so we can actually use a rather
;; coarse-grained selection of revisions to get all the versions of
;; straight.el that we need in order to parse any given recipe format.
;;
;; After we get the appropriate branch name, we directly download that
;; branch's revision of straight.el (the file) from the official
;; GitHub repository, radian-software/straight.el. Now, one of the
;; goals of this implementation is that forking straight.el does not
;; require any changes to this file as long as you do not make
;; divergent changes to the interpretation of recipe fetch keywords.
;; The fact that we download from the source rather than whatever fork
;; is specified does not actually violate this goal, if you think
;; about it. If the fork *does* make divergent changes to recipe
;; interpretation, then all it needs to do is update the version
;; identifier in `straight-freeze-versions', tag a new branch with a
;; unique version identifier, and override the hardcoded
;; radian-software/straight.el to the fork (this last part being
;; necessary because otherwise the new branch could not be found; all
;; previous branches would still be available, though).
;;
;; After we get a copy of straight.el, it's just a matter of using it
;; to parse the recipe for straight.el itself, taking into account all
;; user-set variables. But of course evaluating straight.el will mess
;; up the environment, so we have to do this in a child Emacs
;; instance.
;;
;; Previous versions of straight.el expected a symlink for
;; bootstrap.el, in order for the bootstrap snippet to be able to
;; identify the name of the local repository for straight.el. I
;; decided later that this was more trouble than it was worth, and
;; that users wishing to rename the repository can reasonably be
;; expected to update their bootstrap snippet. We still create the
;; symlink, though, for backwards compatibility (this script should
;; work for all versions of straight.el, present and past).
;;
;; IMPORTANT: When referring to newly introduced variables in
;; straight.el, *always* use `bound-and-true-p' so as to avoid
;; regressions with old versions of straight.el that did not define
;; those variables; see
;; <https://github.com/radian-software/straight.el/issues/407>.

;; We have to wrap everything in a single form so that this file can
;; be evaluated with `eval-print-last-sexp', rather than
;; `eval-buffer', since the latter forcibly swallows all errors(!) and
;; turns them into warnings.
(progn
  (message "Bootstrapping straight.el...")

  ;; Any errors in this Emacs go directly to the user's init-file and
  ;; abort init. Errors in the child Emacs spawned below create a
  ;; non-zero exit code, and are re-thrown.
  (let ((min-version "25.1"))
    (when (version< emacs-version min-version)
      (error (concat "straight.el requires at least Emacs %s, "
                     "but you are running Emacs %s")
             min-version emacs-version)))

  (when (boundp 'straight-repository-branch)
    (unless (stringp straight-repository-branch)
      (error "The `straight-repository-branch' must be a string (was: %S)"
             straight-repository-branch)))

  (unless (executable-find "git")
    (user-error "Git executable not found. straight.el requires git"))

  ;; Load some libraries.
  (require 'cl-lib)
  (require 'url-http)

  ;; Because `url-http' is weird, it doesn't properly define its
  ;; variables. So we have to do this.
  (defvar url-http-end-of-headers)
  (defvar url-http-response-status)

  ;; THIS FUNCTION MUST BE MANUALLY SYNCED WITH
  ;; ./straight.el straight--windows-os-p
  (defun straight--windows-os-p ()
    "Check if the current operating system is Windows."
    (memq system-type '(ms-dos windows-nt)))

  (let ((version nil)
        (straight-profiles (if (boundp 'straight-profiles)
                               straight-profiles
                             '((nil . "default.el"))))
        (straight-install-dir (or (bound-and-true-p straight-base-dir)
                                  user-emacs-directory))
        (print-length nil)
        (print-level nil))
    ;; The only permissible error here is for a lockfile to be absent
    ;; entirely. Anything else triggers an abort so that we don't
    ;; accidentally do something the user doesn't expect (like if they
    ;; forked straight.el and made incompatible divergent changes to
    ;; the recipe specification, and forgot to update which repository
    ;; their init-file downloaded install.el from).
    (dolist (lockfile-name (mapcar #'cdr straight-profiles))
      (let ((lockfile-path (concat straight-install-dir
                                   "straight/versions/"
                                   lockfile-name)))
        (when (file-exists-p lockfile-path)
          (condition-case nil
              (with-temp-buffer
                (insert-file-contents-literally lockfile-path)
                (read (current-buffer))
                (let ((alleged-version (read (current-buffer))))
                  (cond
                   (version
                    (unless (eq alleged-version version)
                      (error (concat "Incompatible recipe versions specified "
                                     "in version lockfiles: %S and %S")
                             version alleged-version)))
                   ((keywordp alleged-version)
                    (setq version alleged-version))
                   (t (error
                       (concat "Invalid recipe version specified "
                               "in version lockfile: %S")
                       alleged-version)))))
            ;; Prevent end-of-file errors from being swallowed (they
            ;; are ignored by default by the debugger).
            (end-of-file
             (error "Malformed version lockfile: %S" lockfile-name))))))
    (unless version
      ;; If no lockfile present, use latest version.
      (setq version :gamma))
    (with-current-buffer
        (url-retrieve-synchronously
         (format
          (concat "https://raw.githubusercontent.com/"
                  "radian-software/straight.el/install/%s/straight.el")
          (substring (symbol-name version) 1))
         'silent 'inhibit-cookies)
      ;; In case of 404, that means the version identifier is unknown.
      ;; This will happen when I screw up, or if the user forks
      ;; straight.el and changes the version identifier, but forgets
      ;; to push a corresponding branch and override the repository
      ;; here.
      (unless (equal url-http-response-status 200)
        (error "Unknown recipe version: %S" version))
      ;; Delete the evil HTTP headers.
      (delete-region (point-min) url-http-end-of-headers)
      ;; All of the following code is actually executed by the child
      ;; Emacs.
      (goto-char (point-min))
      (princ ";; -*- coding: utf-8 -*-" (current-buffer))
      (print
       `(progn
          ;; Pass relevant variables into the child Emacs, if they
          ;; have been set.
          ,@(let* ((vars nil)
                   (regexps '("bootstrap-version"
                              "straight-[a-z-]+"
                              "user-emacs-directory"))
                   (regexp (format "^\\(%s\\)$"
                                   (mapconcat #'identity regexps "\\|"))))
              (mapatoms
               (lambda (sym)
                 (when (and (boundp sym)
                            (string-match-p regexp (symbol-name sym))
                            (not (string-match-p "--" (symbol-name sym))))
                   (push sym vars))))
              (mapcar (lambda (var)
                        `(setq ,var ',(symbol-value var)))
                      vars)))
       (current-buffer))
      (goto-char (point-max))
      (print
       `(progn
          ;; Don't worry, this recipe will be overridden by
          ;; `straight-recipe-overrides' if that variable has been
          ;; set. We're just mirroring bootstrap.el. (But note that
          ;; `:files' doesn't have to be specified, since we're
          ;; skipping the build phase.)
          (straight-use-package-no-build
           `(straight :type git :host github
                      :repo ,(format
                              "%s/straight.el"
                              (or (bound-and-true-p straight-repository-user)
                                  "radian-software"))
                      :branch ,(or (bound-and-true-p
                                    straight-repository-branch)
                                   "master")))
          (unless (and (boundp 'bootstrap-version)
                       (integerp bootstrap-version)
                       (>= bootstrap-version 3))
            ;; Make a bootstrap.el symlink, for backwards compatibility
            ;; (bootstrap snippets versioned 2 and lower expect this
            ;; symlink to exist).
            (let* ((recipe (gethash "straight" straight--recipe-cache))
                   (local-repo (plist-get recipe :local-repo))
                   ;; This is a relative symlink. It won't break if you
                   ;; (for some silly reason) move your
                   ;; `user-emacs-directory'.
                   (link-target (concat "repos/" local-repo "/bootstrap.el"))
                   (link-name (concat straight-install-dir
                                      "straight/bootstrap.el")))
              (ignore-errors
                ;; If it's a directory, the linking will fail. Just let
                ;; the user deal with it in that case, since they are
                ;; doing something awfully weird.
                (delete-file link-name))
              ;; Unfortunately, there appears to be no way to get
              ;; `make-symbolic-link' to overwrite an existing file,
              ;; like 'ln -sf'. Providing the OK-IF-ALREADY-EXISTS
              ;; argument just makes it fail silently in the case of an
              ;; existing file. That's why we have to `delete-file'
              ;; above.
              (if (bound-and-true-p straight-use-symlinks)
                  (if (straight--windows-os-p)
                      (call-process "cmd" nil nil nil "/c" "mklink"
                                    (subst-char-in-string ?/ ?\\ link-name)
                                    (subst-char-in-string ?/ ?\\ link-target))
                    (make-symbolic-link link-target link-name))
                (with-temp-file link-name
                  (print
                   `(load (expand-file-name
                           ,link-target (file-name-directory load-file-name))
                          nil 'nomessage)
                   (current-buffer)))))))
       (current-buffer))
      (let ((temp-file (make-temp-file "straight.el~")))
        (write-region nil nil temp-file nil 'silent)
        (with-temp-buffer
          (unless (= 0
                     (call-process
                      ;; Taken with love from package `restart-emacs'.
                      (let ((emacs-binary-path
                             (expand-file-name
                              invocation-name invocation-directory))
                            (runemacs-binary-path
                             (when (straight--windows-os-p)
                               (expand-file-name
                                "runemacs.exe" invocation-directory))))
                        (if (and runemacs-binary-path
                                 (file-exists-p runemacs-binary-path))
                            runemacs-binary-path
                          emacs-binary-path))
                      nil '(t t) nil
                      "--batch" "--no-window-system" "--quick"
                      "--load" temp-file))
            (error "straight.el bootstrap failed: %s" (buffer-string)))))))

  (message "Bootstrapping straight.el...done"))

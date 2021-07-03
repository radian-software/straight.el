;;; bootstrap.el --- Initialize straight.el -*- lexical-binding: t -*-

;; We need these libraries in this file, bootstrap.el. The libraries
;; for straight.el are required in that file.
(require 'bytecomp)
(require 'cl-lib)

(let* ((bootstrap.el
        ;; If this file is accessed through a symlink (this may happen
        ;; when an old version of the bootstrap snippet is used to
        ;; load straight.el), resolve it. We need to be looking at
        ;; the actual file, since the eventual target of the
        ;; symlink is the only way we can actually identify the
        ;; straight.el repository (which might be called something
        ;; else).
        (file-truename
         (or
          ;; If the file is being loaded from the init-file.
          load-file-name
          ;; If the file is being evaluated with something like
          ;; `eval-buffer'.
          buffer-file-name)))
       (straight.el
        (expand-file-name
         "straight.el" (file-name-directory bootstrap.el))))
  ;; This logic replicates that in `straight--build-compile',
  ;; and is used to silence byte-compile warnings and other cruft.
  (cl-letf (((symbol-function #'save-some-buffers) #'ignore)
            ((symbol-function #'byte-compile-log-1) #'ignore)
            ((symbol-function #'byte-compile-log-file) #'ignore)
            ((symbol-function #'byte-compile-log-warning) #'ignore))
    (let ((byte-compile-warnings nil)
          (byte-compile-verbose nil)
          (message-log-max nil)
          (inhibit-message t)
          (emacs-version-changed t))
      ;; Argument 0 means (for some reason) to byte-compile even if
      ;; the .elc file does not already exist (but not if the .elc
      ;; file is at least as new as the .el file).
      (byte-recompile-file straight.el nil 0)
      (catch 'emacs-version-changed
        ;; straight.el has a fun hack that throws
        ;; `emacs-version-changed' if the version of Emacs has changed
        ;; since the last time it was byte-compiled. This prevents us
        ;; from accidentally loading invalid byte-code, hopefully.
        (load (file-name-sans-extension
               (expand-file-name straight.el default-directory))
              nil 'nomessage)
        (setq emacs-version-changed nil))
      (when emacs-version-changed
        ;; In safe mode, sacrifice performance for safety.
        (if (bound-and-true-p straight-safe-mode)
            (load straight.el nil 'nomessage 'nosuffix)
          ;; Don't use the optional LOAD argument for
          ;; `byte-compile-file' because it emits a message.
          (byte-compile-file straight.el)
          (load (file-name-sans-extension
                 (expand-file-name straight.el default-directory))
                nil 'nomessage))))))

;; This assures the byte-compiler that we know what we are doing when
;; we reference functions and variables from straight.el below. It
;; does not actually do anything at runtime, since the `straight'
;; feature has already been provided by loading straight.elc above.
(require 'straight)

(straight-bootstrap)

;; Then we register (and build) straight.el itself.
(straight-use-package `(straight :type git :host github
                                 :repo ,(format "%s/straight.el"
                                                straight-repository-user)
                                 :files ("straight*.el")
                                 :branch ,straight-repository-branch))

;;; bootstrap.el ends here

;;; bootstrap.el --- Initialize straight.el -*- lexical-binding: t -*-

;; We need these libraries in this file, bootstrap.el. The libraries
;; for straight.el are required in that file.
(require 'bytecomp)
(require 'cl-lib)

;; In Emacs 24.5, `inhibit-message' is not defined, so the
;; byte-compiler complains that it is bound lexically here but not
;; used. We make sure that Emacs 24.5 treats `inhibit-message' as a
;; dynamic variable, even if it does not have any effect.
(defvar inhibit-message)

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
  ;; This logic replicates that in `straight--byte-compile-package',
  ;; and is used to silence byte-compile warnings and other cruft.
  (cl-letf (((symbol-function #'save-some-buffers) #'ignore)
            ((symbol-function #'byte-compile-log-1) #'ignore)
            ((symbol-function #'byte-compile-log-file) #'ignore)
            ((symbol-function #'byte-compile-log-warning) #'ignore))
    (let ((byte-compile-warnings nil)
          (byte-compile-verbose nil)
          (message-log-max nil)
          (inhibit-message t))
      ;; Argument 0 means (for some reason) to byte-compile even if
      ;; the .elc file does not already exist.
      (byte-recompile-file straight.el nil 0)
      ;; Actually load the package manager. This doesn't do anything
      ;; except initialize some caches.
      (load (expand-file-name (concat straight.el "c")
                              default-directory)
            nil 'nomessage 'nosuffix))))

;; This assures the byte-compiler that we know what we are doing when
;; we reference functions and variables from straight.el below. It
;; does not actually do anything at runtime, since the `straight'
;; feature has already been provided by loading straight.elc above.
(require 'straight)

;; In case this is a reinit, and straight.el was already loaded, we
;; have to explicitly clear the caches.
(straight--reset-caches)

;; Treat the first init as a transaction.
(unless (and after-init-time (not (bound-and-true-p straight-treat-as-init)))
  (add-hook 'after-init-hook #'straight-finalize-transaction)
  (straight-begin-transaction)
  (straight-mark-transaction-as-init))

;; We start by registering the default recipe repositories. This is
;; done first so that any dependencies of straight.el can be looked up
;; correctly.

(straight-use-recipes '(org-elpa :local-repo nil))

(straight-use-recipes '(melpa :type git :host github
                              :repo "melpa/melpa"
                              :no-build t))

(straight-use-recipes '(emacsmirror :type git :host github
                                    :repo "emacsmirror/epkgs"
                                    :nonrecursive t
                                    :no-build t))

;; Then we register (and build) straight.el itself.
(straight-use-package `(straight :type git :host github
                                 :repo "raxod502/straight.el"
                                 :files ("straight.el" "straight-x.el")
                                 :branch ,straight-repository-branch))

;;; bootstrap.el ends here

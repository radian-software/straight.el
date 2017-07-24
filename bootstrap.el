;;; bootstrap.el --- Initialize straight.el -*- lexical-binding: t -*-

;; We need these libraries in this file, bootstrap.el. The libraries
;; for straight.el are required in that file.
(require 'bytecomp)
(require 'cl-lib)

(let* ((bootstrap.el
        ;; If this file is accessed through a symlink (the normal
        ;; case), resolve it. We need to be looking at the actual
        ;; file, since the eventual target of the symlink is the only
        ;; way we can actually identify the straight.el repository
        ;; (which might be called something else).
        (file-truename
         (or
          ;; If the file is being loaded from the init-file.
          load-file-name
          ;; If the file is being evaluated with something like
          ;; `eval-buffer' (but please don't do this, as it breaks the
          ;; contract of `straight-declare-init-finished').
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
      (load (concat straight.el "c") nil 'nomessage 'nosuffix))))

;; This assures the byte-compiler that we know what we are doing when
;; we reference functions and variables from straight.el below. It
;; does not actually do anything at runtime, since the `straight'
;; feature has already been provided by loading straight.elc above.
(require 'straight)

;; In case this is a reinit, and straight.el was already loaded, we
;; have to explicitly clear the caches.
(straight--reset-caches)

;; Inform later logic that this is a reinit. However, we only consider
;; a reinit to be like an init if we're guaranteed to know when it
;; ends (i.e., `straight-declare-init-finished' is called at the end
;; of the init-file, even if there is an error).
(when (and after-init-time straight--finalization-guaranteed)
  (setq straight--reinit-in-progress t))

;; Even if finalization was performed correctly last init (or reinit),
;; we don't want to assume that will continue to be the case. Of
;; course, we are proceeding a little bit on faith, since we have no
;; way of knowing that finalization will actually happen just because
;; `straight-declare-init-finished' was called last time. But if we
;; turn out to be wrong, we want to at least correct the error as soon
;; as possible (i.e. the next time init is performed).
(setq straight--finalization-guaranteed nil)

;; If we do a reinit, we still want to use a bulk find(1) command to
;; check for modified packages, since it speeds things up a lot. But
;; if we don't reset the memoized value here, it won't be recomputed
;; and will just be considered stale.
(setq straight--cached-packages-might-be-modified-p :unknown)

;; So meta. This updates the various caches, so that straight.el shows
;; up properly in the lockfile and other things like that.
(straight-use-package '(straight :type git :host github
                                 :repo "raxod502/straight.el"
                                 :files ("straight.el")))

;; Now we need to register the default recipe repositories.

(straight-use-recipes '(melpa :type git :host github
                              :repo "melpa/melpa"
                              :no-build t))

(straight-use-recipes '(emacsmirror :type git :host github
                                    :repo "emacsmirror/epkgs"
                                    :nonrecursive t
                                    :no-build t))

;;; bootstrap.el ends here

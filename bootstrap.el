;;; bootstrap.el --- Initialize straight.el -*- lexical-binding: t -*-

;; We need these libraries in this file, bootstrap.el. The libraries
;; for straight.el are required in that file.
(require 'bytecomp)
(require 'cl-lib)

(let ((straight-dir (expand-file-name
                     (concat
                      user-emacs-directory
                      "straight/repos/straight.el/"))))
  ;; In order for straight.el to be able to require `pbl', we need to
  ;; add it to the load-path.
  (add-to-list 'load-path straight-dir)
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
      (byte-recompile-directory straight-dir 0))))

;; Actually load the package manager. This doesn't do anything except
;; initialize some caches.
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
                                 :repo "raxod502/straight.el"))

;; Now we need to register the default recipe repositories.

(straight-use-recipes `(gnu-elpa :type git
                                 :repo ,straight--recipes-gnu-elpa-url
                                 :local-repo "elpa"
                                 :no-build t))

(straight-use-recipes '(melpa :type git :host github
                              :repo "melpa/melpa"
                              :no-build t))

(straight-use-recipes '(emacsmirror :type git :host github
                                    :repo "emacsmirror/epkgs"
                                    :nonrecursive t
                                    :no-build t))

;;; bootstrap.el ends here

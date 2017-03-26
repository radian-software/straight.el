(let ((straight-dir (expand-file-name
                     (concat
                      user-emacs-directory
                      "straight/repos/straight.el/"))))
  (add-to-list 'load-path straight-dir)
  (let ((message-log-max nil)
        (inhibit-message t))
    (byte-recompile-directory straight-dir 0)))
(require 'straight)
(straight--reset-caches)
(when (and after-init-time straight--finalization-guaranteed)
  (setq straight--reinit-in-progress t))
(setq straight--finalization-guaranteed nil)
(straight-use-package '(straight :fetcher github
                                 :repo "raxod502/straight.el"))

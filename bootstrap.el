(let ((straight-dir (concat
                     user-emacs-directory
                     "straight/repos/straight.el")))
  (add-to-list 'load-path straight-dir)
  (let ((message-log-max nil)
        (inhibit-message t))
    (byte-recompile-directory straight-dir 0)))
(require 'straight)

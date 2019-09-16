;; You can load this file to make sure that forms from other packages
;; are indented correctly.

(put #'use-package-only-one 'lisp-indent-function 'defun)
(put #'use-package-process-keywords #'lisp-indent-function 'defun)

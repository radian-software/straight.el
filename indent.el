;; You can load this file to make sure that forms from other packages
;; are indented correctly. It should cause the indentation to be the
;; same as if you had actually loaded the packages that define these
;; functions.
;;
;; Generally speaking, we use indentation rules from Emacs 30 to
;; indent the straight.el source code.

(put #'if-let 'lisp-indent-function 2)
(put #'when-let 'lisp-indent-function 1)

(put #'use-package-only-one 'lisp-indent-function 'defun)
(put #'use-package-process-keywords 'lisp-indent-function 'defun)

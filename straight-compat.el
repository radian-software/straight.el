;;; straight-compat.el --- Function backports. -*- lexical-binding: t -*-

;; Copyright (C) 2017-2018 Radon Rosborough and contributors

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 1 Jan 2017
;; Homepage: https://github.com/raxod502/straight.el
;; Keywords: extensions
;; Package-Requires: ((emacs "24.4"))
;; Version: prerelease

;;; Commentary:

;; This file contains definitions of functions and macros that do not
;; appear in older versions of Emacs supported by straight.el.

;; It also has a fun hack that allows the bootstrap process to detect
;; whether the Emacs version has changed since straight.el was last
;; byte-compiled.

;; See straight.el for more information.

;;; Code:

;; Hack!
(eval
 `(unless (equal
           (emacs-version)
           ,(eval-when-compile (emacs-version)))
    (throw 'emacs-version-changed nil)))

;; Not defined before Emacs 25.1
(unless (fboundp 'if-let)
  (defmacro if-let (varlist then &optional else)
    "Bind variables according to VARLIST and eval THEN or ELSE.
VARLIST must be of the form ((SYMBOL VALUEFORM)). Evaluate
VALUEFORM and bind it to SYMBOL. If the result of evaluation is
non-nil, evaluate and return THEN. Otherwise, evaluate and return
ELSE (or nil)."
    (let ((symbol (nth 0 (car varlist)))
          (valueform (nth 1 (car varlist))))
      `(let ((,symbol ,valueform))
         (if ,symbol
             ,then
           ,else)))))

;; Not defined before Emacs 25.1
(unless (fboundp 'when-let)
  (defmacro when-let (varlist &rest body)
    "Bind variables according to VARLIST and conditionally eval BODY.
VARLIST must be of the form ((SYMBOL VALUEFORM)). Evaluate
VALUEFORM and bind it to SYMBOL. If the result of evaluation is
non-nil, evaluate and return BODY. Otherwise return nil."
    `(if-let ,varlist
         (progn ,@body))))

;; Not defined before Emacs 25.1
(unless (fboundp 'alist-get)
  (defun alist-get (key alist)
    "Return the value associated with KEY in ALIST, using `assq'."
    (cdr (assq key alist))))

;; Not defined before Emacs 25.3
(unless (boundp 'inhibit-message)
  (defvar inhibit-message nil
    "Non-nil means calls to ‘message’ are not displayed.
They are still logged to the *Messages* buffer."))

(provide 'straight-compat)

;;; straight-compat.el ends here

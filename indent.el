;; You can load this file to make sure that forms from other packages
;; are indented correctly. It should cause the indentation to be the
;; same as if you had actually loaded the packages that define these
;; functions.
;;
;; Generally speaking, we use indentation rules from Emacs 30 to
;; indent the straight.el source code. There are a few tricks to make
;; this work:
;;
;; * Avoid let-binding a variable whose name begins with `def'. Doing
;; so produces incorrect indentation in Emacs 28 and below, due to an
;; overly broad heuristic in the lisp-indent code. If binding a
;; lexical variable, try renaming the variable to something else. If
;; binding a dynamic variable like `default-directory', the
;; indentation is fine as long as the binding fits on a single line.
;; To bind the variable to a longer value, try binding the value to a
;; temporary name first, and then the variable to the temporary name
;; on a single line.
;;
;; * In `cl-labels' and `cl-flet', make the local functions use names
;; which begin with `def'. This will make them indent correctly in
;; Emasc 28 and below, as a helpful side effect of the bug mentioned
;; above. In later versions these forms are always indented correctly.
;;
;; * When writing a data list (i.e., a list that is not a function
;; call) split over multiple lines, put a space between the opening
;; paren and the first element. There is code in Emacs 28 and above to
;; ensure that this convention forces the list to be indented the way
;; you would expect (i.e. with all elements at the same indentation).
;; In this file we backport that logic to Emacs 27 and below. Note
;; that you have to put the first element on the same line as the
;; opening paren for this to work, as later versions can do the same
;; when the first line has a comment instead of the first elements,
;; but earlier versions can't.
;;
;; * When using `define-key', use instead `straight--define-key',
;; which is an alias whose indentation remains the same across Emacs
;; versions, unlike `define-key' which changed in Emacs 29 due to the
;; heuristic change mentioned above. (Avoid `keymap-set', until we
;; drop support for Emacs 28 and prior, where that function doesn't
;; exist yet.)

(put #'if-let 'lisp-indent-function 2)
(put #'when-let 'lisp-indent-function 1)

(put #'use-package-only-one 'lisp-indent-function 'defun)
(put #'use-package-process-keywords 'lisp-indent-function 'defun)

(when (version< emacs-version "28")

  ;; longlines-start
  (defun straight--calculate-lisp-indent-emacs28 (&optional parse-start)
    "Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
If the value is nil, that means don't change the indentation
because the line starts inside a string.

PARSE-START may be a buffer position to start parsing from, or a
parse state as returned by calling `parse-partial-sexp' up to the
beginning of the current line.

The value can also be a list of the form (COLUMN CONTAINING-SEXP-START).
This means that following lines at the same level of indentation
should not necessarily be indented the same as this line.
Then COLUMN is the column to indent to, and CONTAINING-SEXP-START
is the buffer position of the start of the containing expression."
    (save-excursion
      (beginning-of-line)
      (let ((indent-point (point))
            state
            ;; setting this to a number inhibits calling hook
            (desired-indent nil)
            (retry t)
            whitespace-after-open-paren
            calculate-lisp-indent-last-sexp containing-sexp)
        (cond ((or (markerp parse-start) (integerp parse-start))
               (goto-char parse-start))
              ((null parse-start) (beginning-of-defun))
              (t (setq state parse-start)))
        (unless state
          ;; Find outermost containing sexp
          (while (< (point) indent-point)
            (setq state (parse-partial-sexp (point) indent-point 0))))
        ;; Find innermost containing sexp
        (while (and retry
		    state
                    (> (elt state 0) 0))
          (setq retry nil)
          (setq calculate-lisp-indent-last-sexp (elt state 2))
          (setq containing-sexp (elt state 1))
          ;; Position following last unclosed open.
          (goto-char (1+ containing-sexp))
          ;; Is there a complete sexp since then?
          (if (and calculate-lisp-indent-last-sexp
		   (> calculate-lisp-indent-last-sexp (point)))
              ;; Yes, but is there a containing sexp after that?
              (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
					      indent-point 0)))
                (if (setq retry (car (cdr peek))) (setq state peek)))))
        (if retry
            nil
          ;; Innermost containing sexp found
          (goto-char (1+ containing-sexp))
          (setq whitespace-after-open-paren (looking-at (rx whitespace)))
          (if (not calculate-lisp-indent-last-sexp)
	      ;; indent-point immediately follows open paren.
	      ;; Don't call hook.
              (setq desired-indent (current-column))
	    ;; Find the start of first element of containing sexp.
	    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
	    (cond ((looking-at "\\s(")
		   ;; First element of containing sexp is a list.
		   ;; Indent under that list.
		   )
		  ((> (save-excursion (forward-line 1) (point))
		      calculate-lisp-indent-last-sexp)
		   ;; This is the first line to start within the containing sexp.
		   ;; It's almost certainly a function call.
		   (if (or (= (point) calculate-lisp-indent-last-sexp)
                           whitespace-after-open-paren)
		       ;; Containing sexp has nothing before this line
		       ;; except the first element, or the first element is
                       ;; preceded by whitespace.  Indent under that element.
		       nil
		     ;; Skip the first element, find start of second (the first
		     ;; argument of the function call) and indent under.
		     (progn (forward-sexp 1)
			    (parse-partial-sexp (point)
					        calculate-lisp-indent-last-sexp
					        0 t)))
		   (backward-prefix-chars))
		  (t
		   ;; Indent beneath first sexp on same line as
		   ;; `calculate-lisp-indent-last-sexp'.  Again, it's
		   ;; almost certainly a function call.
		   (goto-char calculate-lisp-indent-last-sexp)
		   (beginning-of-line)
		   (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
				       0 t)
		   (backward-prefix-chars)))))
        ;; Point is at the point to indent under unless we are inside a string.
        ;; Call indentation hook except when overridden by lisp-indent-offset
        ;; or if the desired indentation has already been computed.
        (let ((normal-indent (current-column)))
          (cond ((elt state 3)
                 ;; Inside a string, don't change indentation.
	         nil)
                ((and (integerp lisp-indent-offset) containing-sexp)
                 ;; Indent by constant offset
                 (goto-char containing-sexp)
                 (+ (current-column) lisp-indent-offset))
                ;; in this case calculate-lisp-indent-last-sexp is not nil
                (calculate-lisp-indent-last-sexp
                 (or
                  ;; try to align the parameters of a known function
                  (and lisp-indent-function
                       (not retry)
                       (funcall lisp-indent-function indent-point state))
                  ;; If the function has no special alignment
		  ;; or it does not apply to this argument,
		  ;; try to align a constant-symbol under the last
                  ;; preceding constant symbol, if there is such one of
                  ;; the last 2 preceding symbols, in the previous
                  ;; uncommented line.
                  (and (save-excursion
                         (goto-char indent-point)
                         (skip-chars-forward " \t")
                         (looking-at ":"))
                       ;; The last sexp may not be at the indentation
                       ;; where it begins, so find that one, instead.
                       (save-excursion
                         (goto-char calculate-lisp-indent-last-sexp)
		         ;; Handle prefix characters and whitespace
		         ;; following an open paren.  (Bug#1012)
                         (backward-prefix-chars)
                         (while (not (or (looking-back "^[ \t]*\\|([ \t]+"
                                                       (line-beginning-position))
                                         (and containing-sexp
                                              (>= (1+ containing-sexp) (point)))))
                           (forward-sexp -1)
                           (backward-prefix-chars))
                         (setq calculate-lisp-indent-last-sexp (point)))
                       (> calculate-lisp-indent-last-sexp
                          (save-excursion
                            (goto-char (1+ containing-sexp))
                            (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                            (point)))
                       (let ((parse-sexp-ignore-comments t)
                             indent)
                         (goto-char calculate-lisp-indent-last-sexp)
                         (or (and (looking-at ":")
                                  (setq indent (current-column)))
                             (and (< (line-beginning-position)
                                     (prog2 (backward-sexp) (point)))
                                  (looking-at ":")
                                  (setq indent (current-column))))
                         indent))
                  ;; another symbols or constants not preceded by a constant
                  ;; as defined above.
                  normal-indent))
                ;; in this case calculate-lisp-indent-last-sexp is nil
                (desired-indent)
                (t
                 normal-indent))))))
  ;; longlines-end

  (advice-add #'calculate-lisp-indent :override
              #'straight--calculate-lisp-indent-emacs28))

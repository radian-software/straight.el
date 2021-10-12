;;; straight.el --- Tests for straight.el -*- lexical-binding: t; -*-
;;; Code:
(require 'straight)
(require 'ert)
(require 'cus-edit)

(defvar straight-test-docker (getenv "EMACS_DOCKER")
  "Condition tests on docker environment when non-nil.")

(eval-and-compile
  (defmacro straight-test--template (template &optional vars &rest bindings)
    "Return a list of filled TEMPLATEs.
TEMPLATE is an implicitly backquoted form.
VARS should be a list of symbols denoting the destructuring pattern
for BINDINGS."
    (declare (indent 1))
    (if (or (null vars) (null bindings))
        (list template)
      (let ((unbound (mod (length bindings) (length vars))))
        (unless (zerop unbound)
          (error "Unven binding list: %S" (last bindings unbound)))
        (let ((body nil)
              (bindings
               (eval
                `(cl-loop for ,vars on ',bindings
                          by (lambda (l) (nthcdr ,(length vars) l))
                          collect
                          (apply #'append
                                 (cl-mapcar #'list ',vars (list ,@vars)))))))
          (dolist (env bindings (mapcar (lambda (it) (eval it t))
                                        (nreverse body)))
            (unless (mod (length env) 2) (error "Uneven binding list: %S" env))
            (let (e)
              (cl-loop for (var val) on env by #'cddr
                       do (push (list var `(quote ,val)) e))
              (push `(let* ,(nreverse e) (backquote ,template)) body))))))))

(cl-defmacro straight-deftest (object
                               (&key before-each after-each expected-result
                                     doc tags &allow-other-keys)
                               &rest template)
  "Return Auto-tagged and documented `ert-deftest' for OBJECT with TEMPLATE."
  (declare (indent defun))
  (let ((counter 0)
        (autotags
         (delq nil
               (list
                (if (string-match-p "--" (symbol-name object))
                    'private 'public)
                (if (macrop object) 'macro))))
        (tests (when template
                 (macroexpand `(straight-test--template ,@template)))))
    (setq tags (append autotags tags))
    `(progn
       ,@(mapcar
          (lambda (test)
            `(ert-deftest
                 ,(intern (concat
                           (format "%s/test" object)
                           (when (> (length tests) 1)
                             (format "@%d" (cl-incf counter)))))
                 ()
               ,(or doc (when (fboundp object) (documentation object)))
               ,@(when tags `(:tags ',tags))
               ,@(when expected-result `(:expected-result ,expected-result))
               ,@(when before-each before-each)
               ,test
               ,@(when after-each after-each)))
          tests))))

(defun straight-test-enable-fontlocking ()
  "Enable fontlocking for `straight-deftest'."
  (font-lock-add-keywords
   nil
   '(("(\\(\\<straight-deftest\\)\\>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t)))))
(straight-test-enable-fontlocking)

;;adapted from json.el's json-alist-p
(defun straight--alist-p (list &optional restriction)
  "Return non-nil if LIST is an alist.
RESTRICTION is an optionaly unary function which tests each element of
the list. If any element returns nil when passed to RESTRICTION,
return nil."
  (while (and (consp (car-safe list))
              (atom (caar list))
              (funcall (if restriction restriction #'identity)
                       (car-safe list))
              (setq list (cdr list))))
  (null list))

(defun straight--hash-equal (a b)
  "Return t if hash A and B are equal, else nil."
  (and (= (hash-table-count a) (hash-table-count b))
       (catch 'flag
         (maphash (lambda (x y) (or (equal (gethash x b) y)
                                    (throw 'flag nil)))
                  a)
         t)))

;;; Tests

;; Tests which assert a comparison should consistinently compare
;; EXPECTED to ACTUAL. This makes reading otuput of failed tests
;; easier. e.g. (should (eq EXPECTED ACTUAL))

;; Test default types of customizations.
(dolist (group (cons 'straight
                     (mapcar #'car (custom-group-members 'straight t))))
  (let ((vars (mapcar #'car (cl-remove 'custom-group
                                       (custom-group-members group nil)
                                       :key #'cadr))))
    ;;CI env does not have install-info or makeinfo installed
    (when straight-test-docker
      (setq vars (cl-remove-if
                  (lambda (var)
                    (member var '(straight-makeinfo-executable
                                  straight-install-info-executable)))
                  vars)))
    (dolist (var vars)
      (eval
       `(straight-deftest ,var
          (:doc
           ,(format "%S default value matches :type specification" var)
           :tags (custom))
          (should (eq t
                      (widget-apply
                       (widget-convert (custom-variable-type ',var))
                       :match
                       (eval (car (get ',var 'standard-value)) t)))))))))

(ert-deftest straight--functionp ()
  (let ((fn #'straight-use-package))
    (should (straight--functionp fn)))
  (declare-function straight--not-a-function "straight-test") ;;stub
  (let ((fn #'straight--not-a-function))
    (should-not (straight--functionp fn))))

(ert-deftest straight--executable-find ()
  (declare-function executable-find "files.el")
  (should (equal (executable-find "emacs")
                 (straight--executable-find "emacs")))
  (should-error (straight--executable-find "not-an-executable")))

(straight-deftest straight--flatten
  ( :tags (compatibility)
    :before-each ((skip-unless (version<= "27.1" emacs-version))))
  (should (equal (flatten-tree ',in) (straight--flatten ',in)))
  (in)
  t nil () (()) 1 ((1)) ((1) (2)) (((1))))

(straight-deftest straight--alist-set ()
  (should (equal ',out (straight--alist-set ,@in)))
  (in                                  out)
  ("a" 'b  '(("c" . d)))               (("a" . b) ("c" . d))
  ("a" 'd  '(("a" . b) ("a" . c)))     (("a" . d) ("a" . c))
  ("a" 'a  '((a . b) (a . c)) 'symbol) (("a" . a) (a . b) (a . c)))

(straight-deftest straight--with-plist ()
  (should (eq 8 (let ((plist '(:a 1 :b 2 :c 3)))
                  (straight--with-plist plist (a b ((:c d)) (e 2))
                    (+ a b d e))))))

(straight-deftest straight--put ()
  (should (equal (let ((plist '(:a 1 :b 2 :c 3)))
                   (straight--put plist :a 0) plist)
                 '(:a 0 :b 2 :c 3))))

(straight-deftest straight--remq ()
  (let* ((plist '(:a 1 :b 2 "string" 3))
         (original (copy-tree plist)))
    (ignore original) ;;pacify byte-compiler
    (should (equal (straight--remq plist ',in) ,out)))
  (in        out)
  (:a :b)    '("string" 3)
  ("string") original)

(straight-deftest straight--plist-get ()
  (let ((plist '(:a 1 :b 2 :c 3)))
    (should (equal (straight--plist-get plist ,@in) ',out)))
  (in           out)
  (:a nil)      1
  (:d 'default) default)

(straight-deftest straight--insert ()
  ;;Don't use #s reader syntax here. We want to ensure fresh tables.
  (let ((table      (make-hash-table :test #'equal))
        (test-table (make-hash-table :test #'equal)))
    ;;initialize tables
    (puthash "a" '("b") test-table)
    (cl-loop for (key val) on ',out by #'cddr
             do (puthash key val table))
    (should (straight--hash-equal
             (straight--insert ,@in test-table) table)))
  (in         out)
  (0 "c" "d") ("a" ("b") "c" ("d"))
  (2 "a" "c") ("a" ("b" nil "c")))

(straight-deftest straight--checkhash ()
  (let ((table (make-hash-table :test #'equal)))
    (puthash "found" '("not-a-key") table)
    (should (eq (straight--checkhash ,in table) ,out)))
  (in         out)
  "found"     t
  "not-a-key" nil)

(straight-deftest straight-vc-git--fork-repo ()
  (let ((recipe '( :package "package"
                   :host    github
                   :repo    "upstream/repo"))
        (straight-host-usernames
         '((github    . "githubUser")
           (gitlab    . "gitlabUser")
           (bitbucket . "bitbucketUser"))))
    (should (equal (straight-vc-git--fork-repo
                    (plist-put recipe :fork ',in))
                   ,out)))
  (in                                  out)
  nil                                  "upstream/repo"
  t                                    "githubUser/repo"
  "fork"                               "fork/repo"
  "fork/"                              "fork/repo"
  "/rename"                            "githubUser/rename"
  "fork/rename"                        "fork/rename"
  (:host bitbucket)                    "bitbucketUser/repo"
  (:repo "/rename")                    "githubUser/rename"
  (:host github :repo "user/")         "user/repo"
  (:host gitlab :repo "full/override") "full/override"
  ;; https://github.com/raxod502/straight.el/issues/592
  (:host nil :repo "/local/repo")      "/local/repo"
  (:branch "feature")                  "githubUser/repo")

(straight-deftest straight--build-steps ()
  (let* ((defaults
           (mapcar (lambda (sym)
                     (intern (string-remove-prefix "straight-disable-"
                                                   (symbol-name sym))))
                   straight--build-default-steps))
         (straight-disable-info
          (member 'info           ,disabled))
         (straight-disable-compile
          (member 'compile        ,disabled))
         (straight-disable-autoloads
          (member 'autoloads      ,disabled))
         (straight-disable-native-compile
          (member 'native-compile ,disabled)))
    (ignore defaults) ;;pacify byte-compiler
    (should (equal (mapcar
                    (lambda (step)
                      (intern (format "straight--build-%s"
                                      (symbol-name step))))
                    ,steps)
                   (straight--build-steps
                    '(:package "test" :build ,build)))))
  (disabled       build            steps)
  nil             nil              nil
  defaults        nil              nil
  nil             t                defaults
  defaults        t                defaults
  nil             (compile)        '(compile)
  defaults        (autoloads info) '(autoloads info)
  nil             (:not compile)   '(autoloads native-compile info)
  defaults        (:not compile)   nil
  '(info)         (:not compile)   '(autoloads native-compile))

(provide 'straight-test)

;;; straight-test.el ends here

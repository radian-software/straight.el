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
                object
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
               ,@(when before-each (if (cl-every #'listp before-each)
                                       before-each
                                     (list before-each)))
               ,test
               ,@(when after-each (if (cl-every #'listp after-each)
                                      after-each
                                    (list after-each)))))
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
;; easier. e.g. (should (eq EXPECTED ACTUAL)).

;;;; Customizations
(dolist (group (cons 'straight
                     (mapcar #'car (custom-group-members 'straight t))))
  (let ((vars (mapcar #'car (cl-remove 'custom-group
                                       (custom-group-members group nil)
                                       :key #'cadr))))
    ;; CI env does not have install-info or makeinfo installed.
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

(defun straight-test--mock-file (&rest segments)
  "SEGMENTS."
  (expand-file-name
   (string-join segments "/")
   (expand-file-name "./tests/mocks/"
                     ;; These tests can be run interactively from this
                     ;; file, and from the Makefile in the parent
                     ;; directory.
                     (locate-dominating-file "./" "straight.el"))))

(defvar straight-test-mock-user-emacs-dir
  (straight-test--mock-file ".emacs.d/")
  "Mock `user-emacs-dir'.")

(defun straight-test-trim-to-mocks (path)
  "Trim PATH up to and including './mocks'."
  (string-remove-prefix
   (file-name-as-directory (straight-test--mock-file)) path))

;;;; Unit Tests
(straight-deftest straight--add-package-to-info-path
  (:before-each (defvar Info-directory-list))
  (let ((straight-base-dir straight-test-mock-user-emacs-dir)
        (Info-directory-list '()))
    (straight--add-package-to-info-path
     '(:package "straight-mock-repo" :local-repo "./test-repo"))
    ;; No "dir" file, so this directory does not get added
    (should (null Info-directory-list))))

(straight-deftest straight--alist-set ()
  (should (equal ',out (straight--alist-set ,@in)))
  (in                                  out)
  ("a" 'b  '(("c" . d)))               (("a" . b) ("c" . d))
  ("a" 'd  '(("a" . b) ("a" . c)))     (("a" . d) ("a" . c))
  ("a" 'a  '((a . b) (a . c)) 'symbol) (("a" . a) (a . b) (a . c)))

(straight-deftest straight--autoloads-file ()
  (let ((straight-base-dir straight-test-mock-user-emacs-dir))
    (should (string= ".emacs.d/straight/build/test/test-autoloads.el"
                     (straight-test-trim-to-mocks
                      (straight--autoloads-file "test"))))))

(straight-deftest straight--build-cache-file ()
  (let ((straight-base-dir straight-test-mock-user-emacs-dir))
    (should (string= ".emacs.d/straight/build-cache.el"
                     (straight-test-trim-to-mocks
                      (straight--build-cache-file))))))

(straight-deftest straight--build-dir ()
  (let ((straight-base-dir straight-test-mock-user-emacs-dir))
    (should (string= (file-name-as-directory
                      (format ".emacs.d/straight/build/%s" ,in))
                     (straight-test-trim-to-mocks
                      (straight--build-dir ,in)))))
  (in) "" "test")

(straight-deftest straight--build-disabled-p ()
  (should (eq ,out (straight--build-disabled-p ',recipe)))
  (recipe      out)
  ()           nil
  (:build t)   nil
  (:build nil) t)

(straight-deftest straight--build-file ()
  (let ((straight-base-dir straight-test-mock-user-emacs-dir))
    (should (string= (format ".emacs.d/straight/build/%s" ,in)
                     (straight-test-trim-to-mocks
                      (straight--build-file ,in)))))
  (in) "test.el")

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
    (should (equal (mapcar (lambda (step)
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

(straight-deftest straight--buildable-p ()
  (should (eq ,out (straight--buildable-p ',in)))
  (in          out)
  ()           t
  (:build t)   t
  (:build nil) nil)

;;@TODO: mock plain symbol that triggers recipe lookup
(straight-deftest straight--convert-recipe ()
  (let ((straight-recipe-repositories nil))
    (should (equal ',out (straight--convert-recipe ',in))))
  (in out)
  (doct :repo "progfolio/doct" :fetcher github)
  ( :repo "progfolio/doct"
    :fetcher github
    :package "doct"
    :type git
    :local-repo "doct")
  emacs
  (:type built-in :package "emacs"))

(straight-deftest straight--catching-quit ()
  (,assert (straight--catching-quit ,in))
  (assert      in)
  should       t
  should-not   (signal 'quit nil)
  should-error (signal 'wrong-type-argument nil))

(straight-deftest straight--checkhash ()
  (let ((table (make-hash-table :test #'equal)))
    (puthash "found" '("not-a-key") table)
    (should (eq ,out (straight--checkhash ,in table))))
  (in         out)
  "found"     t
  "not-a-key" nil)

(straight-deftest straight--dependencies ()
  (let ((straight--build-cache (make-hash-table :test #'equal)))
    (cl-loop for (key val) on ',build-cache by #'cddr
             do (puthash key val straight--build-cache))
    (should (equal ',dependencies (straight--dependencies))))
  (build-cache                                       dependencies)
  ("p" ())                                           nil
  ("p" (nil ("emacs")))                              nil
  ("p" (nil ("dependency")))                         ("dependency")
  ("p" (nil ("a" "b" "c")))                          ("a" "b" "c")
  ("p" (nil ("a" "b" "c")) "p2" (nil ("b" "c" "d"))) ("a" "b" "c" "d"))

(straight-deftest straight--dir ()
  (let ((straight-base-dir straight-test-mock-user-emacs-dir))
    (should (string= (file-name-as-directory
                      (format ".emacs.d/straight/%s" ,in))
                     (straight-test-trim-to-mocks (straight--dir ,in)))))
  (in) "" "test")

(straight-deftest straight--directory-files ()
  (cl-flet ((mock (&rest args) (apply #'straight-test--mock-file args)))
    (let ((default-directory straight-test-mock-user-emacs-dir))
      (should (equal ,files (straight--directory-files ,@args)))))
  (args                           files)
  ()                              '("straight")
  ("../../" ".*.el")              '("straight-test.el")
  ((mock) nil 'full)              `(,(mock ".emacs.d"))
  ((mock ".emacs.d" "straight") nil nil #'string<) '("build" "repos"))

(straight-deftest straight--emacs-path ()
  (should (equal (concat invocation-directory invocation-name)
                 (straight--emacs-path))))

(straight-deftest straight--ensure-blank-lines ()
  (cl-flet ((buffer-with-point-at (s n)
              (with-temp-buffer
                (insert s)
                (goto-char (point-min))
                (when (search-forward "|" nil t)
                  (delete-region (match-beginning 0)
                                 (match-end 0)))
                (straight--ensure-blank-lines n)
                (buffer-string))))
    (should (equal ,buffer-string (buffer-with-point-at ,string ,n))))
  (string n buffer-string)
  "|beginning-of-buffer" 1 "beginning-of-buffer"
  "a|b" 1 "a\nb"
  "a|b" 2 "a\n\nb")

(straight-deftest straight--emacs-dir ()
  (let ((straight-base-dir straight-test-mock-user-emacs-dir))
    (should (string= (file-name-as-directory
                      (format ".emacs.d/%s" ,in))
                     (straight-test-trim-to-mocks
                      (straight--emacs-dir ,in)))))
  (in) "" "test")

(straight-deftest straight--emacs-file ()
  (let ((straight-base-dir straight-test-mock-user-emacs-dir))
    (should (string= ,out
                     (straight-test-trim-to-mocks
                      (straight--emacs-file ,@in)))))
  (in       out)
  ("a")     ".emacs.d/a"
  ("a" "b") ".emacs.d/a/b")

(straight-deftest straight--file ()
  (let ((straight-base-dir straight-test-mock-user-emacs-dir))
    (should (string= ,out
                     (straight-test-trim-to-mocks
                      (straight--file ,@in)))))
  (in       out)
  ("a")     ".emacs.d/straight/a"
  ("a" "b") ".emacs.d/straight/a/b")

(ert-deftest straight--functionp ()
  (let ((fn #'straight-use-package))
    (should (straight--functionp fn)))
  (declare-function straight--not-a-function "straight-test") ; stub
  (let ((fn #'straight--not-a-function))
    (should-not (straight--functionp fn))))

(ert-deftest straight--executable-find ()
  (declare-function executable-find "files.el")
  (should (equal (executable-find "emacs")
                 (straight--executable-find "emacs")))
  (should-error (straight--executable-find "not-an-executable")))

;;@Incomplete: 'melpa style expansions, cons cells not tested yet
(straight-deftest straight--expand-files-directive-internal ()
  (let* ((straight-base-dir straight-test-mock-user-emacs-dir)
         (mock-repo (straight--repos-dir "straight-mock-repo"))
         (default-directory mock-repo))
    (should (equal ',out (straight--expand-files-directive-internal
                          ,@in))))
  (in                    out)
  ('()                   mock-repo nil nil) (nil)
  ('()                   mock-repo "p" nil) (nil)
  ('("*.none")           mock-repo nil nil) (nil)
  ('("nonexistant-file") mock-repo nil nil) (nil)

  ('(:defaults) mock-repo nil nil)
  ((("not.el" . "not.el") ("straight-mock-repo.el" . "straight-mock-repo.el")))

  ('(:defaults) mock-repo "prefix/" nil)
  ((("not.el" . "prefix/not.el")
    ("straight-mock-repo.el" . "prefix/straight-mock-repo.el")))

  ('("*.el") mock-repo nil nil)
  ((("not.el" . "not.el") ("straight-mock-repo.el" . "straight-mock-repo.el")))

  ('("subdir/*.el") mock-repo nil nil)
  ((("subdir/a.el" . "a.el") ("subdir/b.el" . "b.el")))

  ('(:defaults (:exclude "not.el")) mock-repo nil nil)
  ((("straight-mock-repo.el" . "straight-mock-repo.el"))
   "not.el")

  ('((:exclude "*")) mock-repo nil nil)
  (nil "not.el" "straight-mock-repo.el" "subdir"))

(straight-deftest straight--flatten
  ( :tags (compatibility)
    :before-each (skip-unless (version<= "27.1" emacs-version)))
  (should (equal (flatten-tree ',in) (straight--flatten ',in)))
  (in)
  t nil () (()) 1 ((1)) ((1) (2)) (((1))))

(straight-deftest straight--get-dependencies ()
  (let ((straight--build-cache (make-hash-table :test #'equal))
        (data '("p" () "p2" (nil ("emacs")) "p3" (nil ("p2")))))
    (cl-loop for (key val) on data by #'cddr
             do (puthash key val straight--build-cache))
    (should (equal ',dependencies (straight--get-dependencies ,package))))
  (package dependencies)
  "p"      nil
  "p2"     ("emacs")
  ;; Doesn't resolve transitive dependencies on its own.
  "p3"     ("p2"))

(straight-deftest straight--get-overridden-recipe ()
  (let ((straight-profiles '((test . nil)))
        (straight-recipe-overrides '((test . ((package t))))))
    (should (equal '(package t) (straight--get-overridden-recipe 'package)))))

(straight-deftest straight--get-transitive-dependencies ()
  (let ((straight--build-cache (make-hash-table :test #'equal))
        (data '("p" () "p2" (nil ("emacs")) "p3" (nil ("p2")))))
    (cl-loop for (key val) on data by #'cddr
             do (puthash key val straight--build-cache))
    ;; If we don't sort the return values are inconsistent on Emacs 25
    (should (equal (sort (copy-tree ',dependencies) #'string<)
                   (sort (straight--get-transitive-dependencies ,package)
                         #'string<))))
  (package dependencies)
  "p"      ("p")
  "p2"     ("p2" "emacs")
  "p3"     ("p3" "p2" "emacs"))

(straight-deftest straight--with-plist ()
  (should (eq 8 (let ((plist '(:a 1 :b 2 :c 3)))
                  (straight--with-plist plist (a b ((:c d)) (e 2))
                    (+ a b d e))))))

(straight-deftest straight--put ()
  (should (equal '(:a 0 :b 2 :c 3)
                 (let ((plist '(:a 1 :b 2 :c 3)))
                   (straight--put plist :a 0) plist))))

(straight-deftest straight--remq ()
  (let* ((plist '(:a 1 :b 2 "string" 3))
         (original (copy-tree plist)))
    (ignore original) ;;pacify byte-compiler
    (should (equal ,out (straight--remq plist ',in))))
  (in        out)
  (:a :b)    '("string" 3)
  ("string") original)

(straight-deftest straight--process-call ()
  (should (equal ',out (straight--process-call ,@args)))
  (args                                        out)
  ("echo" "hi")                                (0 "hi\n" nil)
  ("bash" "-c" "echo err 1>&2")                (0 nil "err\n")
  ("bash" "-c" "straight.el-test &>/dev/null") (127 nil nil))

(straight-deftest straight--plist-get ()
  (let ((plist '(:a 1 :b 2 :c 3)))
    (should (equal ',out (straight--plist-get plist ,@in))))
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
    (should (straight--hash-equal table (straight--insert ,@in test-table))))
  (in         out)
  (0 "c" "d") ("a" ("b") "c" ("d"))
  (2 "a" "c") ("a" ("b" nil "c")))

(straight-deftest straight--modified-dir ()
  (let ((straight-base-dir straight-test-mock-user-emacs-dir))
    (should (string= (file-name-as-directory
                      (format ".emacs.d/straight/modified/%s" ,in))
                     (straight-test-trim-to-mocks
                      (straight--modified-dir ,in)))))
  (in) "" "test")

(straight-deftest straight--modified-file ()
  (let ((straight-base-dir straight-test-mock-user-emacs-dir))
    (should (string= (format ".emacs.d/straight/modified/%s" ,in)
                     (straight-test-trim-to-mocks
                      (straight--modified-file ,in)))))
  (in) "test.el")

(straight-deftest straight--mtimes-dir ()
  (let ((straight-base-dir straight-test-mock-user-emacs-dir))
    (should (string= (file-name-as-directory
                      (format ".emacs.d/straight/mtimes/%s" ,in))
                     (straight-test-trim-to-mocks
                      (straight--mtimes-dir ,in)))))
  (in) "" "test")

(straight-deftest straight--mtimes-file ()
  (let ((straight-base-dir straight-test-mock-user-emacs-dir))
    (should (string= (format ".emacs.d/straight/mtimes/%s" ,in)
                     (straight-test-trim-to-mocks
                      (straight--mtimes-file ,in)))))
  (in) "test.el")

(straight-deftest straight--path-prefix-p ()
  (should (straight--path-prefix-p
           (straight-test--mock-file) (straight-test--mock-file ".emacs.d"))))

(straight-deftest straight--repos-dir ()
  (let ((straight-base-dir straight-test-mock-user-emacs-dir))
    (should (string= (file-name-as-directory
                      (format ".emacs.d/straight/repos/%s" ,in))
                     (straight-test-trim-to-mocks
                      (straight--repos-dir ,in)))))
  (in) "" "test")

(straight-deftest straight--repos-file ()
  (let ((straight-base-dir straight-test-mock-user-emacs-dir))
    (should (string= (format ".emacs.d/straight/repos/%s" ,in)
                     (straight-test-trim-to-mocks
                      (straight--repos-file ,in)))))
  (in) "test.el")

(straight-deftest straight--uniquify ()
  (should (equal ,out (straight--uniquify ,@args)))
  (args                       out)
  ("test" '())                "test"
  ("test" '("test"))          "test-1"
  ("test" '("test" "test-1")) "test-2")

(straight-deftest straight-vc-git--fork-repo ()
  (let ((recipe '( :package "package"
                   :host    github
                   :repo    "upstream/repo"))
        (straight-host-usernames
         '((github    . "githubUser")
           (gitlab    . "gitlabUser")
           (bitbucket . "bitbucketUser"))))
    (should (equal ,out (straight-vc-git--fork-repo
                         (plist-put recipe :fork ',in)))))
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
  ;; https://github.com/radian-software/straight.el/issues/592
  (:host nil :repo "/local/repo")      "/local/repo"
  (:branch "feature")                  "githubUser/repo")

(straight-deftest straight-vc-git--decode-url ()
  (should (equal ,out (straight-vc-git--decode-url ,in)))
  (in                                  out)
  "x://x.y/user/x.git"                 '("x://x.y/user/x.git" nil nil)
  "git@github.com:user/test.git"       '("user/test" github ssh)
  "https://codeberg.org/user/test.git" '("user/test" codeberg https)
  "git@codeberg.org:user/test.git"     '("user/test" codeberg ssh)
  "git@git.sr.ht:~user/test"           '("user/test" sourcehut ssh)
  "https://git.sr.ht/~user/test"       '("user/test" sourcehut https))

(straight-deftest straight-vc-git--encode-url ()
  (let ((straight-vc-git-default-protocol 'https))
    (should (equal ,out (straight-vc-git--encode-url "user/repo" ,@in))))
  (in               out)
  (nil)             "user/repo"
  ('github)         "https://github.com/user/repo.git"
  ('github 'ssh)    "git@github.com:user/repo.git"
  ('codeberg)       "https://codeberg.org/user/repo.git"
  ('codeberg 'ssh)  "git@codeberg.org:user/repo.git"
  ('sourcehut 'ssh) "git@git.sr.ht:~user/repo")

(straight-deftest straight--versions-dir ()
  (let ((straight-base-dir straight-test-mock-user-emacs-dir))
    (should (string= (file-name-as-directory
                      (format ".emacs.d/straight/versions/%s" ,in))
                     (straight-test-trim-to-mocks
                      (straight--versions-dir ,in)))))
  (in) "" "test")

(straight-deftest straight--versions-file ()
  (let ((straight-base-dir straight-test-mock-user-emacs-dir))
    (should (string= (format ".emacs.d/straight/versions/%s" ,in)
                     (straight-test-trim-to-mocks
                      (straight--versions-file ,in)))))
  (in) "test.el")

(straight-deftest straight--watcher-dir ()
  (let ((straight-base-dir straight-test-mock-user-emacs-dir))
    (should (string= (file-name-as-directory
                      (format ".emacs.d/straight/watcher/%s" ,in))
                     (straight-test-trim-to-mocks
                      (straight--watcher-dir ,in)))))
  (in) "" "test")

(straight-deftest straight--watcher-file ()
  (let ((straight-base-dir straight-test-mock-user-emacs-dir))
    (should (string= (format ".emacs.d/straight/watcher/%s" ,in)
                     (straight-test-trim-to-mocks
                      (straight--watcher-file ,in)))))
  (in) "test.el")

(provide 'straight-test)

;; Local Variables:
;; compile-command: "make -C ../"
;; create-lockfiles: nil
;; auto-save-default: nil
;; End:

;;; straight-test.el ends here

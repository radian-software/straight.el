;;; straight.el --- Tests for straight.el -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'straight)

(defvar straight-test-default-recipe '( :package "package"
                                        :host github :type git
                                        :repo "upstream/repo"))

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
  (and (= (hash-table-count a)
          (hash-table-count b))
       (catch 'flag (maphash (lambda (x y)
                               (or (equal (gethash x b) y)
                                   (throw 'flag nil)))
                             a)
              t)))

(describe "defcustom :type specifications"
  (dolist (group
           (cons 'straight (mapcar #'car (custom-group-members 'straight t))))
    (dolist (var
             (mapcar
              #'car
              (cl-remove
               'custom-group
               (custom-group-members group nil)
               :key #'cdr)))
      (describe (format "%S" var)
        (it "has a default value matching its :type specification"
          (expect
           (widget-apply (widget-convert (custom-variable-type var))
                         :match (eval (car (get var 'standard-value)) t))
           :to-be t))))))

(describe "straight--alist-set"
  (describe "(key val alist)"
    (it "adds (KEY . VAL) to front of ALIST"
      (expect (straight--alist-set "a" 'b  '(("c" . d)))
              :to-equal '(("a" . b) ("c" . d))))
    (it "sets first KEY match cdr"
      (expect (straight--alist-set "a" 'd  '(("a" . b) ("a" . c)))
              :to-equal '(("a" . d) ("a" . c)))))
  (describe "(key val alist symbol)"
    (it "tests with eq when SYMBOL non-nil"
      (expect (straight--alist-set "a" 'a  '((a . b) (a . c)) 'symbol)
              :to-equal '(("a" . a) (a . b) (a . c))))))

(describe "straight--with-plist"
  (describe "(plist props body)"
    (it "correctly binds PROPS and executes BODY"
      (expect (let ((plist '(:a 1 :b 2 :c 3)))
                (straight--with-plist plist (a b ((:c d)) (e 2))
                  (+ a b d e)))
              :to-equal 8))))

(describe "straight--put"
  (describe "(plist prop value)"
    (it "sets PROP to VALUE on PLIST"
      (expect (let ((plist '(:a 1 :b 2 :c 3)))
                (straight--put plist :a 0)
                plist)
              :to-equal '(:a 0 :b 2 :c 3)))))

(describe "straight--remq"
  (describe "(plist props)"
    (it "removes PROPS from PLIST"
      (expect (let ((plist '(:a 1 :b 2 :c 3)))
                (straight--remq plist '(:a :b))
                plist)
              :to-equal '(:c 3)))
    (it "compares PROPS with `eq'"
      (expect (let ((plist '("test" "PASS")))
                ;;this should not match the key
                (straight--remq plist '("test"))
                plist)
              :to-equal '("test" "PASS")))))

(describe "straight--plsit-get"
  (describe "(plist prop default)"
    (it "returns PROP from PLIST"
      (expect (let ((plist '(:a 1 :b 2 :c 3)))
                (straight--plist-get plist :a nil))
              :to-equal 1))
    (it "uses DEFAULT if PROP not found on PLIST"
      (expect (let ((plist '(:a 1 :b 2 :c 3)))
                (straight--plist-get plist :d 2))
              :to-equal 2))))

(describe "straight--insert"
  (describe "(n key value table)"
    (it "associates Nth index in KEY with VALUE"
      (expect
       (let ((table
              #s(hash-table size 1 test equal data ("p1" ("d1")))))
         (straight--hash-equal
          (straight--insert 0 "p2" "d2" table)
          #s( hash-table size 2 test equal rehash-size 1.5
              rehash-threshold 0.8125 data
              ("p1" ("d1") "p2" ("d2")))))
       :to-be t))
    (it "pads with nil if Nth entry does not exist"
      (expect
       (let ((table #s(hash-table size 1 test equal data ("p1" ("d1")))))
         (straight--hash-equal
          (straight--insert 2 "p1" "d2" table)
          #s( hash-table size 2 test equal rehash-size 1.5
              rehash-threshold 0.8125 data
              ("p1" ("d1" nil "d2"))))))
      :to-be t)))

(describe "straight--checkhash"
  (describe "(key table)"
    (it "Retruns non-nil if KEY is present in hash TABLE"
      (expect
       (let ((table #s(hash-table size 1 test equal data ("p1" ("d1")))))
         (straight--checkhash "p1" table))
       :to-be t))
    (it "Retruns nil if KEY is not present in hash TABLE"
      (expect
       (let ((table #s(hash-table size 1 test equal data ("p1" ("d1")))))
         (straight--checkhash "p2" table))
       :to-be nil)))
  (describe "(key table paranoid)"
    (it "ensures correctness for TABLE containing straight--not-present")))

(describe "straight-vc-git--fork-repo"
  (before-each (setq straight-host-usernames
                     '((github . "githubUser")
                       (gitlab . "gitlabUser")
                       (bitbucket . "bitbucketUser"))))
  (describe ":fork nil"
    (it "returns inherited upstream :repo"
      ;; explicit
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe :fork nil))
              :to-equal "upstream/repo")
      ;; implicit
      (expect (straight-vc-git--fork-repo straight-test-default-recipe)
              :to-equal "upstream/repo")))
  (describe ":fork t"
    (it "returns \"straight-host-username/upstream-repo-name\""
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe :fork t))
              :to-equal "githubUser/repo")))
  (describe ":fork string"
    (it "returns \"STRING/upstream-repo\" with suffixed or absent \"/\""
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe :fork "fork/"))
              :to-equal "fork/repo")
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe :fork "fork"))
              :to-equal "fork/repo"))
    (it "returns \"straight-host-username/STRING\" when prefixed with \"/\""
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe :fork "/rename"))
              :to-equal "githubUser/rename"))
    (it "returns \"STRING\" when \"/\" that is not a prefix or suffix"
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe :fork "fork/rename"))
              :to-equal "fork/rename")))
  (describe ":fork plist"
    (it "looks up :fork PLIST's :host"
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe
                          :fork '(:host bitbucket)))
              :to-equal "bitbucketUser/repo"))
    (it "gives precedence to :fork PLIST's :repo"
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe
                          :fork '(:repo "/rename")))
              :to-equal "githubUser/rename")
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe
                          :fork '(:host github :repo "user/")))
              :to-equal "user/repo")
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe
                          :fork '(:host gitlab :repo "full/override")))
              :to-equal "full/override")
      ;; https://github.com/raxod502/straight.el/issues/592
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe
                          :fork '(:host nil :repo "/local/repo")))
              :to-equal "/local/repo"))
    (it "looks up the user when :fork PLIST does not provide a :repo"
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe
                          :fork '(:branch "feature")))
              :to-equal "githubUser/repo"))))

(describe "straight--build-steps"
  (describe ":build nil"
    (it "Returns an empty list"
      (expect (straight--build-steps '(:package "test" :build nil))
              :to-be nil)))
  (describe ":build t"
    (it "Returns the default list of build step functions"
      (expect (straight--build-steps '(:package "test" :build t))
              :to-have-same-items-as '(straight--build-autoloads
                                       straight--build-compile
                                       straight--build-native-compile
                                       straight--build-info)))
    (it "Ignores straight-disable-SYMBOL options"
      (expect (let ((straight-disable-autoloads t)
                    (striaght-disable-compile t)
                    (straight-disable-native-compile t)
                    (straight-disable-info t))
                (straight--build-steps '(:package "test" :build t))
                :to-have-same-items-as '(straight--build-autoloads
                                         straight--build-compile
                                         straight--build-native-compile
                                         straight--build-info)))))
  (describe ":build (steps...)"
    (it "Returns straight--build-SYMBOL functions in declared order"
      (expect (straight--build-steps '(:package "test" :build (compile)))
              :to-equal '(straight--build-compile))
      (expect (straight--build-steps
               '(:package "test" :build (autoloads info)))
              :to-equal '(straight--build-autoloads straight--build-info))))
  (describe ":build (:not steps...)"
    (it "Returns the default steps less each step in the list"
      (expect (straight--build-steps
               '(:package "test" :build (:not compile)))
              :to-equal '(straight--build-autoloads
                          straight--build-native-compile
                          straight--build-info)))
    (it "Respects straight-disable-SYMBOL options"
      (expect (let ((straight-disable-info t))
                (straight--build-steps
                 '(:package "test" :build (:not compile))))
              :to-equal '(straight--build-autoloads
                          straight--build-native-compile))))
  (describe "no :build declared"
    (it "Returns the default steps"
      (expect (straight--build-steps '(:package "test"))
              :to-equal '(straight--build-autoloads
                          straight--build-compile
                          straight--build-native-compile
                          straight--build-info)))
    (it "Respects straight-disable-SYMBOL options"
      (expect (let ((straight-disable-info t))
                (straight--build-steps '(:package "test")))
              :to-equal '(straight--build-autoloads
                          straight--build-compile
                          straight--build-native-compile)))))

(provide 'straight-test)

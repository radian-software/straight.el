;;; straight.el --- Tests for straight.el -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'straight)

(defvar straight-test-default-recipe '( :package "package"
                                        :host github :type git
                                        :repo "upstream/repo"))
(defun straight--hash-equal (a b)
  "Return t if hash A and B are equal, else nil."
  (and (= (hash-table-count a)
          (hash-table-count b))
       (catch 'flag (maphash (lambda (x y)
                               (or (equal (gethash x b) y)
                                   (throw 'flag nil)))
                             a)
              t)))

(describe "straight--normalize-alist"
  (describe "(alist)"
    (it "does not mutate ALIST"
      (expect (let ((alist '((a . b) (a . c))))
                (straight--normalize-alist '((a . b) (a . c)))
                alist)
              :to-equal '((a . b) (a . c))))
    (it "removes duplicate keys"
      (expect (straight--normalize-alist '((a . b) (a . c)))
              :to-equal '((a . c)))))
  (describe "(alist test)"
    (it "respects TEST function"
      (expect (straight--normalize-alist '(("a" . b) ("a" . c)) #'equal)
              :to-equal '(("a" . c))))))

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

(provide 'straight-test)

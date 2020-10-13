;;; straight.el --- Tests for straight.el -*- lexical-binding: t; -*-
;; Package-Requires: ((buttercup))

;; Created: 06 Oct 2020
;; Version: prerelease


;;; Commentary:
;;

;;; Code:
(require 'buttercup)
(require 'straight)

(defvar straight-test-default-recipe '( :package "package"
                                        :host github :type git
                                        :repo "upstream/repo"))
(describe "straight.el"
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
                :to-equal "githubUser/repo")))))


(provide 'straight-test)

;; Local Variables:
;; End:


;;; test-straight.el ends here

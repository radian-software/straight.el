# straight.el

> The straightforward package manager for Emacs.

This package is a work-in-progress attempt to completely replace
`package.el` and `quelpa` with a more flexible and powerful package
management system.

## Getting started

Because `straight.el` is meant to replace `package.el` entirely, you
cannot install it through `package.el`. Instead, add the following
bootstrap code to your `init.el`:

    (let ((repos-dir (concat user-emacs-directory "straight/repos/")))
      (unless (file-exists-p (concat repos-dir "straight.el"))
        (make-directory repos-dir 'parents)
        (message "Cloning repository \"straight.el\"...")
        (unless (= 0 (call-process
                      "git" nil nil nil "clone" "--recursive"
                      "https://github.com/raxod502/straight.el.git"
                      (expand-file-name
                       (concat repos-dir "straight.el"))))
          (error "Could not clone straight.el"))
        (message "Cloning repository \"straight.el\"...done"))
      (load (concat repos-dir "straight.el/bootstrap.el")
            nil 'nomessage))

If you want to use `use-package`, go ahead and install it:

    (straight-use-package 'use-package)

Henceforth you can use `use-package` to declare your packages, and
`straight.el` will install them for you:

    (use-package transpose-frame)

You can also provide a recipe for packages that are not in either
MELPA or GNU ELPA:

    (use-package el-patch
      :recipe (:fetcher github
               :repo "raxod502/el-patch"))

If you don't use `use-package`, you can provide recipes the
old-fashioned way:

    (straight-use-package '(el-patch :fetcher github
                                     :repo "raxod502/el-patch"))


## VCS

* Git
* Mercurial
* Bazaar
* Subversion
* CVS
* Fossil
* Darcs
* EmacsWiki

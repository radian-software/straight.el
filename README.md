# straight.el

The **straightforward package manager** for [Emacs][emacs]. Download
and run packages **directly from source**. Jump straight to package
code and hack away with **no manual rebuilds** required.

## Put it in your init-file

Add the following code to **initialize the package management
system**:

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

To **improve efficiency**, put the following code somewhere it is
**guaranteed to be run** every time your init-file is loaded, **even
if there is an error**:

    (straight-declare-init-finished)

To gain access to **additional features**, put the following code at
the end of your init-file, so it is only run if init finishes
**without errors**:

    (straight-declare-init-succeeded)

## Install packages

`straight.el` has **out-of-the-box support**
for [MELPA][melpa], [GNU ELPA][gnu elpa],
and [Emacsmirror][emacsmirror]. Any package from those repositories
can be installed and activated **immediately** by adding a
`straight-use-package` form to your init-file and evaluating it
(`C-M-x` or `C-x C-e`):

    (straight-use-package 'cider)          ; from MELPA
    (straight-use-package 'seq)            ; only available on GNU ELPA
    (straight-use-package 'a2ps-multibyte) ; only available from Emacsmirror

To install a package **temporarily**, run `M-x straight-use-package`
and select a package from the list.

## But what about my fork of (random package)?

`straight.el` makes it **trivial to hack** on packages. If you have
**your own fork** of [`projectile`][projectile] on GitHub, specify it
like this:

    (straight-use-package '(projectile :host github
                                       :repo "your-name/projectile"))

Evaluating this **won't overwrite the existing repository**, if you
already installed Projectile -- `straight.el` never messes with your
code. **To switch to your fork**, all you have to do is **add a
remote** directly to the Git repository:

    ~/.emacs.d/straight/repos/projectile

`straight.el` allows you to **freely commit and branch** in your
package's Git repositories.

## Integration with `use-package`

`straight.el` comes with **out-of-the-box integration** with
`use-package`. First, install `use-package`:

    (straight-use-package 'use-package)

Now `:ensure` will **automatically use `straight.el`**:

    (use-package projectile
      :ensure t)

You can **provide a recipe** via the `:recipe` keyword:

    (use-package projectile
      :ensure t
      :recipe (:fetcher github
               :repo "your-name/projectile"))

Specifying `:ensure` is unnecessary if you **add this configuration**:

    (setq use-package-always-ensure t)

`straight.el` is a drop-in **replacement for `quelpa-use-package`**.
Simply replace `:quelpa` with `:recipe`. It also has support for
`use-package` **deferred installation** with `:defer-install`.

## Update packages

To **update your packages**, simply run `M-x straight-update-package`
or `M-x straight-update-all`.

`straight.el` aims to allow **complete reproducibility** for your
configuration. To **export the versions** of all packages in use, run
`M-x straight-freeze-versions`. To **revert to the versions listed**
in the exported file, run `M-x straight-thaw-versions`. (In future,
`straight.el` will automatically checkout the listed versions of
packages when you run your config on a fresh machine. Coming
Soon&trade;)

[emacs]: https://www.gnu.org/software/emacs/
[emacsmirror]: https://emacsmirror.net/
[gnu elpa]: https://elpa.gnu.org/
[melpa]: http://melpa.org/#/
[projectile]: https://github.com/bbatsov/projectile
[recipe format]: https://github.com/melpa/melpa#recipe-format

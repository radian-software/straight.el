**straight.el**: next-generation package manager for [Emacs].

<!-- toc -->

- [Features](#features)
- [Guiding principles](#guiding-principles)
- [Getting started](#getting-started)
  * [Install packages](#install-packages)
  * [But what about my fork of (obscure .el package)?](#but-what-about-my-fork-of-obscure-el-package)
  * [Integration with `use-package`](#integration-with-use-package)
  * [Edit packages locally](#edit-packages-locally)
  * [Automatic repository management](#automatic-repository-management)
  * [Configuration reproducibility](#configuration-reproducibility)
- [Conceptual overview](#conceptual-overview)
  * [What is a package?](#what-is-a-package)
  * [Where do packages come from?](#where-do-packages-come-from)
  * [What does this look like on disk?](#what-does-this-look-like-on-disk)
  * [Where do repositories come from?](#where-do-repositories-come-from)
  * [What does it mean to load a package?](#what-does-it-mean-to-load-a-package)
  * [Where do recipes come from?](#where-do-recipes-come-from)
  * [What happens when I call `straight-use-package`?](#what-happens-when-i-call-straight-use-package)
  * [What does it mean to register a package?](#what-does-it-mean-to-register-a-package)
  * [How does `straight.el` know when to rebuild packages?](#how-does-straightel-know-when-to-rebuild-packages)
  * [How does `straight.el` know what packages are installed?](#how-does-straightel-know-what-packages-are-installed)
- [Comparison to other package managers](#comparison-to-other-package-managers)
- [User manual](#user-manual)
- [Implementation details](#implementation-details)
- [Trivia](#trivia)
  * [Comments and docstrings](#comments-and-docstrings)

<!-- tocstop -->

## Features

* Install Emacs packages from MELPA, GNU ELPA, EmacsMirror, or
  manually specified sources.
* Clone and manage packages as Git (or other) repositories, not as
  opaque tarballs.
* Make changes to a package simply by editing its source code, no
  additional steps required. Contribute upstream just by pushing your
  changes.
* Powerful interactive workflows (with popups à la Magit) for
  performing bulk maintenance on the Git repositories for your
  packages.
* Save and load version lockfiles that ensure 100% reproducibility for
  your Emacs configuration. Package state is defined entirely by your
  init-file and (optional) lockfile, with no extra persistent data
  floating around.
* Specify package descriptions using a powerful recipe format that
  supports everything from [MELPA recipes][melpa-recipe-format] and
  more.
* First-class support for deferred and conditional installation, as
  well as [use-package] integration.
* Modular: you can install your packages manually and straight.el will
  load them for you. Or you can also have straight.el install your
  packages, while you provide the recipes explicitly. Or straight.el
  can also fetch recipes, if you want. Bulk repository management and
  package updates are also optional.
* Extensible APIs to add new recipe sources and version-control
  backends.
* The cleanest source code you've ever
  seen. [45%][comments-and-docstrings] of `straight.el` is comments
  and docstrings.

## Guiding principles

* Init-file and version lockfiles as the sole source of truth. No
  persistent state kept elsewhere.
* 100% reproducible package management, accounting for changes in
  packages, recipe repositories, configuration, and the package
  manager itself.
* No support whatsoever for package.el.
* Edit packages by editing their code, no extra steps required. Allow
  for manual version control operations.
* Compatibility with MELPA, GNU ELPA, and EmacsMirror.
* Trivial to quickly try out a package without permanently installing
  it.

## Getting started

First, place the following bootstrap code in your init-file:

    (let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
          (bootstrap-version 1))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (delete-region (point-min) url-http-end-of-headers)
          (eval-buffer)))
      (load bootstrap-file nil 'nomessage))

Even if you want to use a particular version or branch of
`straight.el`, or even your own fork, this code does not need to be
modified. To learn more, see the documentation
on [configuring the installation of straight.el][straight.el-recipe].

It is recommended but not required to call two functions in your
init-file: `straight-declare-init-succeeded` and
`straight-declare-init-finished`.

* Put the following code somewhere it is *guaranteed to be run* every
  time your init-file is loaded, *even if there is an error*:

        (straight-declare-init-finished)

  You can do this by wrapping your init-file in a `condition-case`
  statement. (It is generally desirable to do this anyway, since this
  allows you to provide graceful error handling.)

* Put the following code at the end of your init-file, so that it will
  only be run if init finishes *without errors*:

        (straight-declare-init-succeeded)

For information about why you want to do this, see the documentation
on [the init lifecycle][init-lifecycle].

### Install packages

Out of the box, you can install any package from
[MELPA], [GNU ELPA][gnu-elpa], or [EmacsMirror]. To install a package
temporarily (until you restart Emacs), run `M-x straight-use-package`
and select the package you want. To install a package permanently,
place a call to `straight-use-package` in your init-file, like:

    (straight-use-package 'el-patch)

Note that installing a package will activate all of its autoloads, but
it will not actually `require` the features provided by the package.
This means that you might need to use `require` or `autoload` for some
antiquated packages that do not properly declare their autoloads.

To learn more, see the documentation
on [the package lifecycle][package-lifecycle].

### But what about my fork of (obscure .el package)?

Instead of passing just a package name to `straight-use-package`, you
can pass a list ("recipe"). You can see the default recipe for any
given package by running `M-x straight-get-recipe`. For example, the
recipe for `el-patch` is:

    (el-patch :type git :host github :repo "raxod502/el-patch")

So, if you have forked `el-patch` and you want to use your fork
instead of the upstream, do:

    (straight-use-package
     '(el-patch :type git :host github :repo "your-name/el-patch"))

In fact, `straight.el` has explicit support for using a forked
package, since this is so common:

    (straight-use-package
     '(el-patch :type git :host github :repo "your-name/el-patch"
                :upstream (:host github
                           :repo "raxod502/el-patch")))

You may also omit the `:type git` if you leave `straight-default-vc`
at its default value of `git`.

To learn more, see the documentation
on [the recipe format][recipe-format].

### Integration with `use-package`

[`use-package`][use-package] is a macro that provides convenient
syntactic sugar for many common tasks related to installing and
configuring Emacs packages. Of course, it does not actually install
the packages, but instead defers to a package manager, like
`straight.el` (which comes with `use-package` integration by default).

To use `use-package`, first install it with `straight.el`:

    (straight-use-package 'use-package)

Now `use-package` will use `straight.el` to automatically install
missing packages if you provide `:ensure t`:

    (use-package el-patch
      :ensure t)

You can still provide a custom recipe for the package using the
`:recipe` keyword:

    (use-package el-patch
      :ensure t
      :recipe (el-patch :type git :host github :repo "your-name/el-patch"
                        :upstream (:host github
                                   :repo "raxod502/el-patch")))

Specifying `:ensure` is unnecessary if you set
`use-package-always-ensure` to a non-nil value.

`use-package` has support for deferred package installation, and this
is also supported by `straight.el`. For example:

    (use-package dockerfile-mode
      :ensure t
      :defer-install t
      :mode "Dockerfile.*\\'")

When you first open a `Dockerfile`, you will be prompted `Install
package "dockerfile-mode"?`. If you answer yes, the package will be
installed and you will immediately have syntax highlighting and all
the other features of `dockerfile-mode`.

To learn more, see the documentation
on
[`straight.el`'s `use-package` integration][use-package-integration].

### Edit packages locally

One of the biggest strengths of `straight.el` is that editing packages
locally is trivial. You literally just edit the files (`find-function`
and friends all work as you would expect). Packages will be
automatically rebuilt if necessary when Emacs next starts up.

You can even commit your changes and push or pull to various remotes
using Git. You have complete control over your packages' Git
repositories.

To learn more, see the documentation
on [the package lifecycle][package-lifecycle].

### Automatic repository management

While being able to make arbitrary changes to your packages is very
powerful, it can also get tiring to keep track of the all those
changes. For this reason, `straight.el` provides a suite of powerful
commands to interactively perform bulk operations on your packages.

* To restore each package to its canonical state (a clean working
  directory with the main branch checked out, and the remotes set
  correctly), run `M-x straight-normalize-package` or `M-x
  straight-normalize-all`.

* To pull from each package's configured remote, run `M-x
  straight-pull-package` or `M-x straight-pull-all`. To also pull from
  the upstream, if one is configured, provide a prefix argument.

* To push all local changes to each package's configured remote, run
  `M-x straight-push-package` or `M-x straight-push-all`.

All of these commands are highly interactive and ask you before making
any changes. At any point, you can stop and perform manual operations
with Magit or other tools in a recursive edit.

To learn more, see the documentation
on [bulk repository management][repository-management].

### Configuration reproducibility

To save the currently checked out revisions of all of your packages,
run `M-x straight-freeze-versions`. The resulting file
(`~/.emacs.d/straight/versions/default.el`), together with your
init-file, perfectly define your package configuration. Keep your
version lockfile checked into version control; when you install your
Emacs configuration on another machine, the versions of packages
specified in your lockfile will automatically be checked out after the
packages are installed. You can manually revert all packages to the
revisions specified in the lockfile by running `M-x
straight-thaw-versions`.

To learn more, see the documentation
on [version lockfiles][lockfiles].

## Conceptual overview

This section describes, at a high level, how the different mechanisms
in `straight.el` play together. This illustrates how `straight.el`
manages to accomplish all of
its [guiding principles][guiding-principles].

### What is a package?

A *package* is a collection of Emacs Lisp (and possibly other) files.
The most common case is just a single `.el` file, but some packages
have many `.el` files, and some even have a directory structure.

Note that a package is defined only as a collection of files. It
doesn't necessarily correspond to a Git repository, or an entry on
MELPA, or anything like that. Frequently there is a relationship
between all of these concepts, but that relationship does not always
have to be direct or one-to-one.

A package also has a name, which must be unique. This is the name that
is used for the folder holding the package's files. It is frequently
the same as the name of a Git repository, or an entry on MELPA, but
again this does not have to be the case.

### Where do packages come from?

If you really wanted all of your packages to be unambiguously defined,
you could just copy and paste all of their files into version control.
But that would defeat the purpose of using a package manager like
`straight.el`. In `straight.el`, packages are defined by two sources
of information:

* a *local repository*
* a *build recipe*

The local repository is just a directory containing some files. Of
course, it also has a name, which may or may not be the same as the
package's name. Frequently, the local repository is also a Git
repository, but this is not necessary.

The build recipe is not a literal data structure. It is a concept that
represents a certain subset of the package's recipe. Specifically, the
`:files`, `:local-repo`, and `:no-build` keywords.

To transform this *information* into an actual package that Emacs can
load, `straight.el` *builds* the package. This means that some
symbolic links are created in the package's directory that point back
into the local repository's directory. Exactly how these symlinks are
created is determined by the `:files` directive, and which local
repository the symlinks point to is determined by the `:local-repo`
directive.

After the symlinks are created, the resulting files are byte-compiled,
and their autoloads are generated and written into a file in the
package's directory.

If `:no-build` is specified, however, this entire process is skipped.
This mechanism is used for recipe repositories.

### What does this look like on disk?

The local repositories are kept in `~/.emacs.d/straight/repos`, and
the built packages are kept in `~/.emacs.d/straight/build`. If you
have initialized `straight.el` and loaded package `el-patch`, then
your `~/.emacs.d/straight` directory will look roughly like this (some
irrelevant details have been omitted for pedagogical purposes):

    straight
    ├── build
    │   ├── el-patch
    │   │   ├── el-patch-autoloads.el
    │   │   ├── el-patch.el -> ~/.emacs.d/straight/repos/el-patch/el-patch.el
    │   │   └── el-patch.elc
    │   └── straight
    │       ├── straight-autoloads.el
    │       ├── straight.el -> ~/.emacs.d/straight/repos/straight.el/straight.el
    │       └── straight.el
    └── repos
        ├── el-patch
        │   ├── CHANGELOG.md
        │   ├── LICENSE.md
        │   ├── README.md
        │   └── el-patch.el
        └── straight.el
            ├── LICENSE.md
            ├── Makefile
            ├── README.md
            ├── bootstrap.el
            ├── install.el
            └── straight.el

As you can see, the package names are `el-patch` and `straight`. While
`el-patch` is built from a local repository of the same name,
`straight` is built from a local repository by the name `straight.el`.
Also note that only `.el` files are symlinked, since only they are
relevant to Emacs.

### Where do repositories come from?

Local repositories provide a way to define packages without specifying
the contents of all of their files explicitly. But that's not helpful
without a higher-level way to define local repositories without
specifying the contents of all of *their* files. In `straight.el`,
local repositories are defined by two sources of information:

* a *fetch recipe*
* the *version lockfiles*

The fetch recipe is, like the build recipe, a concept representing a
certain subset of the package's overall recipe. The situation is more
interesting here because `straight.el` supports multiple
version-control backends. The version-control backend specified by the
fetch recipe is determined by the `:type` directive (which defaults to
`straight-default-vc`). Each version-control backend then accepts some
set of additional directives. For example, the `git` backend accepts:

* `:repo`
* `:host`
* `:branch`
* `:nonrecursive`
* `:upstream`

If a local repository is not present, then its fetch recipe describes
how to obtain it. This is done using the `straight--vc-clone`
function, which delegates to one of the backend implementations of the
`clone` operation, according to `:type`.

However, even with a particular repository source specified, there is
still the question of which version of the repository to use. This is
where the version lockfiles come in. When a local repository is
cloned, the version lockfiles are searched to see if there is a
particular commit specified for that local repository's name. If so,
that commit is checked out. (For the `git` backend, commits are
40-character strings representing SHA-1 hashes, but the representation
of a commit identifier could be different across different backends.)

The `straight-freeze-versions` and `straight-thaw-versions` methods
also use backend-delegating methods; in this case, they are
`straight--vc-get-commit` and `straight--vc-check-out-commit`.

The fetch recipe and version lockfiles together, combined with the
configuration options for `straight.el`, precisely define the state of
a local repository. Of course, you may make any changes you want to
the local repository. But this information defines a "canonical" state
that you may revert to at any time.

When this information is combined with the build recipe, `straight.el`
is able to construct canonical, universal versions of your Emacs
packages that will be the same everywhere and forever.

Note that you do not have to provide fetch recipes or version
lockfiles. You may manage your local repositories manually, if you
wish, although this has obvious disadvantages in terms of
repeatability and maintainability.

### What does it mean to load a package?

A prerequisite to loading a package is making sure the package has
been built. After that is done, loading the package means adding its
directory to the load path and evaluating its autoloads file.

Adding the directory to the load path means that you can use `require`
to load the package's files. Note that `straight.el` does not do this
for you, since loading packages immediately is usually not necessary
and it immensely slows down Emacs startup.

Evaluating the autoloads file means that calling the functions that
are defined in the autoloads file will automatically `require` the
files that define those functions. All modern packages define their
functions in autoloads and are designed to be loaded on-demand when
those functions are called. Antiquated packages may need you to
explicitly define autoloads, or to just `require` the package right
away.

### Where do recipes come from?

`straight-use-package` does not require an actual recipe. You can just
give it a package name, and it will look up the recipe. This is done
using *recipe repositories*. Recipe repositories are set up as a
swappable backend system, much like the version-control backend
system.

A recipe repository consists of four parts:

* a fetch recipe for the local repository (this will typically include
  the `:no-build` directive, since recipe repositories usually do not
  need to be built)
* a function that, provided the local repository is already available,
  returns a list of all packages that have recipes in the recipe
  repository
* a function that, given a package name, returns the recipe for that
  package, or nil if the recipe repository does not provide a recipe
  for the package
* an entry in `straight-recipe-repositories` indicating that the
  recipe provided actually corresponds to a recipe repository
  (otherwise it would just be a regular package)

Note that recipe repositories are implemented as regular packages!
This means that all the usual package management operations work on
them as well. It also means that you use `straight-use-package` to
register them (although typically you will provide arguments to
`straight-use-package` so that the recipe repository is only
registered, and not cloned until it is needed;
see
[the section on `straight-use-package`][straight-use-package-overview]).

If you give `straight-use-package` just a package name, then each
recipe repository in `straight-recipe-repositories` is checked for a
recipe for that package. Once one is found, it is used. Otherwise, an
error is signaled (unless the package is built-in to Emacs, according
to package.el).

Note that `straight.el` uses its own recipe format which is similar,
but not identical, to the one used by MELPA. The recipe repository
backends abstract over the formatting differences in different recipe
sources to translate recipes into the uniform format used by
`straight.el`. When you run `M-x straight-get-recipe`, the translated
recipe is what is returned.

### What happens when I call `straight-use-package`?

There are three actions that `straight-use-package` can take:

* Register a package's recipe with `straight.el`.
* Clone a package's local repository, if it is missing.
* Build a package, if it has been changed since the last time it was
  built, and load it.

These actions must be performed in order. Depending on the arguments
you pass to `straight-use-package`, one, two, or all three may be
performed.

The normal case is to do all three. The fetch recipe is only required
if the local repository is actually missing, but the build recipe is
always required.

Deferred installation can be accomplished by telling
`straight-use-package` to stop if the local repository is not already
available. The deferred installation can be triggered by invoking
`straight-use-package` again, but telling it to go ahead and clone the
repository (this is the default behavior). Because
`straight-use-package` already registered the package's recipe the
first time, you don't have to provide it again.

In some extraordinary circumstances (such as when `straight.el` is
bootstrapping its own installation), it may be desirable to clone a
package's local repository if it is missing, but to stop before
building and loading the package. This can also be done by
`straight-use-package`.

### What does it mean to register a package?

Package registration is the first action taken by
`straight-use-package`, before building and cloning. First, if only a
package name was provided to `straight-use-package`, a recipe is
obtained from the configured recipe repositories. Next, the resulting
recipe is recorded in various caches.

This is important, since it allows for several things to happen:

* if you later want to perform another operation on the package using
  `straight.el`, you do not need to provide the recipe again
* if you use a custom recipe for Package A, and Package B requires
  Package A as a dependency, your custom recipe is remembered and
  re-used when Package A is used as a dependency, to avoid conflicts.
* when multiple packages are built from the same local repository, and
  you have specified a custom fetch recipe for one of those packages,
  `straight.el` can intelligently merge that fetch recipe into the
  automatically retrieved recipes of dependencies, in order to avoid
  conflicts.
* `straight.el` knows which packages you have installed, if you want
  to perform interactive operations on them.
* if you accidentally provide two different recipes for the same
  package, `straight.el` can issue a helpful warning, since this may
  lead to surprising behavior.

### How does `straight.el` know when to rebuild packages?

When you request for `straight.el` to load a package (using
`straight-use-package`), it first checks if the package needs to be
rebuilt. This means that some of the files in its local repository
have been modified since the last time the package was built.
`straight.el` uses an optimized `find(1)` command to check for package
modifications, and it uses some caching mechanisms to perform bulk
`find(1)` operations on multiple packages, to speed up these checks
(although it never performs optimizations that may result in erroneous
behavior).

This check occurs during Emacs init, when your init-file makes calls
to `straight-use-package`. You may notice a significant delay on the
first `straight-use-package` call, because this is when `straight.el`
performs a bulk `find(1)` call and caches the results for later usage
(this speeds up init considerably). The total delay is likely to be on
the order of 100ms for a double-digit number of packages.

The rebuild detection system is what allows for you to make changes to
packages whenever you would like, without performing any additional
operations.

(Packages are also rebuilt when their recipes change, of course.)

### How does `straight.el` know what packages are installed?

`straight.el` does not require you to declare a central list of
packages anywhere, like Cask does. Instead, it determines what
packages are to be loaded implicitly, by your invocations of
`straight-use-package` during Emacs initialization. Furthermore,
`straight.el` allows you to install packages after initialization
using `M-x straight-install-package` (or even by evaluating
`straight-use-package` forms). However, `straight.el` still provides
advanced features such as bulk package management and version locking.
This creates some interesting challenges which other package managers
do not have to deal with.

`straight.el` solves these problems using a concept called the *init
lifecycle*. Specifically, it treats your init-file like a function
call that returns the packages that have been registered. This
function is "called" when Emacs loads your init-file during
initialization, and it is "called" again whenever you reload your
init-file. It "returns" a value implicitly, by means of `straight.el`
keeping track of when init begins and ends, as well as when and how
`straight-use-package` was called during init.

You communicate the init lifecycle to `straight.el` using two
functions, `straight-declare-init-finished` and
`straight-declare-init-succeeded`. They are used for different
purposes.

* Since you call `straight-declare-init-succeeded` when your init-file
  has loaded successfully in its entirety, this defines a set of
  packages for `straight.el`. Everything from when `straight.el` is
  bootstrapped (or the bootstrap code is invoked again, if you
  re-loaded your init-file) to when `straight-declare-init-succeeded`
  is called is considered as part of init, and `straight.el` defines
  your packages to be the packages that were registered using
  `straight-use-package` during init.

  If `straight-use-package` is called after init, then this
  invalidates the knowledge that `straight.el` has of your packages.
  Why? It is impossible to know whether the new package that has been
  installed is supposed to be part of your Emacs configuration or not.
  Perhaps you added a `straight-use-package` form to your init-file
  permanently, and have just evaluated it for convenience, or perhaps
  you called `M-x straight-use-package` to test out a package
  temporarily. (Or you could have evaluated a temporary
  `straight-use-package` to temporarily install a package from a
  custom source.)

  If you attempt to perform an action like `M-x
  straight-freeze-versions` after the package data is invalidated,
  `straight.el` will ask you to re-load your init-file first. This
  allows `straight.el` to compute the list of packages from scratch,
  and determine whether that new package was actually supposed to be
  part of your Emacs configuration (based on whether it was registered
  during that re-init).

  Now, calling `straight-declare-init-succeeded` is not actually
  required. However, it does mean that `straight.el` has no way of
  distinguishing packages that are part of your Emacs configuration
  from ones that were installed ad-hoc (other than simply asking the
  user, which is exactly what `straight.el` does if you do not bother
  to call `straight-declare-init-succeeded`).

* While `straight-declare-init-succeeded` is mostly for validation
  purposes, `straight-declare-init-finished` is for optimization
  purposes. Since `straight.el` keeps track of information about built
  packages persistently (to avoid rebuilding packages unnecessarily),
  it must write its build cache to disk. By default, it has no way of
  knowing whether another instance of Emacs has written to the build
  cache between invocations of `straight-use-package`, so it must load
  and save the build cache from disk once for each package. This is
  slow.

  However, there is an obvious optimization: simply load the build
  cache at the first invocation of `straight-use-package`, and save it
  "later", after everything is "finished". This clearly does not work
  in general, but it does work during Emacs initialization (and
  re-initialization), provided we assume that only one instance of
  Emacs is starting up at the same time, and provided that we have
  some way of determining that init has finished.

  This is where `straight-declare-init-finished` comes in: it tells
  `straight.el` when init has finished (as the name suggests). We
  cannot use `straight-declare-init-succeeded` for this purpose, since
  the build cache needs to be written even when there is an error
  (otherwise, all the packages that were built on the run with the
  error would be rebuilt the next time, because `straight.el` would
  not realize that they had already been built; this would lead to
  slow debugging).

  There is one obvious concern with this scheme: how does
  `straight.el` know that `straight-declare-init-finished` is actually
  going to be called? Luckily, it does not need to know during init:
  it can use `after-init-hook` instead. And after init has finished,
  `straight.el` knows whether `straight-declare-init-finished` was
  called, and assumes that the same will happen on a re-init.

## Comparison to other package managers

FIXME

## User manual

FIXME

## Implementation details

FIXME

## Trivia

This section has random, (possibly) interesting tidbits about
`straight.el` that don't fit in the other sections.

### Comments and docstrings

How did I get that statistic about the percentage of `straight.el`
that is comments and docstrings? Simple: by abusing the syntax
highlighting.

    (let ((lines (make-hash-table :test #'equal)))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when (memq (face-at-point)
                    '(font-lock-comment-face
                      font-lock-doc-face))
          (puthash (line-number-at-pos) t lines))
        (forward-char))
      (* (/ (float (length (hash-table-keys lines)))
            (line-number-at-pos))
         100))

Note that you will have to scroll through the entire buffer first,
since `font-lock-mode` computes syntax highlighting lazily.

[comments-and-docstrings]: #comments-and-docstrings
[guiding-principles]: #guiding-principles
[init-lifecycle]: FIXME
[lockfiles]: FIXME
[package-lifecycle]: FIXME
[recipe-format]: FIXME
[repository-management]: FIXME
[straight-use-package-overview]: #what-happens-when-i-call-straight-use-package
[straight.el-recipe]: FIXME
[use-package-integration]: FIXME

[emacs]: https://www.gnu.org/software/emacs/
[emacsmirror]: https://emacsmirror.net/
[gnu-elpa]: https://elpa.gnu.org/
[melpa-recipe-format]: https://github.com/melpa/melpa#recipe-format
[melpa]: http://melpa.org/#/
[use-package]: https://github.com/jwiegley/use-package

**straight.el**: next-generation, purely functional package manager
for the [Emacs] hacker.

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
  * [TL;DR](#tldr)
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
  * [TL;DR](#tldr-1)
  * [Comparison to `package.el`](#comparison-to-packageel)
    + [Advantages of `straight.el`](#advantages-of-straightel)
    + [Advantages of `package.el`](#advantages-of-packageel)
    + [Additional notes](#additional-notes)
  * [Comparison to Quelpa](#comparison-to-quelpa)
    + [Advantages of `straight.el`](#advantages-of-straightel-1)
    + [Advantages of Quelpa](#advantages-of-quelpa)
    + [Additional notes](#additional-notes-1)
  * [Comparison to el-get](#comparison-to-el-get)
    + [Advantages of `straight.el`](#advantages-of-straightel-2)
    + [Advantages of el-get](#advantages-of-el-get)
  * [Comparison to Cask](#comparison-to-cask)
    + [Advantages of `straight.el`](#advantages-of-straightel-3)
    + [Advantages of Cask](#advantages-of-cask)
  * [Comparison to Borg](#comparison-to-borg)
    + [Advantages of `straight.el`](#advantages-of-straightel-4)
    + [Advantages of Borg](#advantages-of-borg)
  * [Comparison to the manual approach](#comparison-to-the-manual-approach)
    + [Advantages of `straight.el`](#advantages-of-straightel-5)
    + [Advantages of the manual approach](#advantages-of-the-manual-approach)
- [User manual](#user-manual)
- [Developer manual](#developer-manual)
- [Trivia](#trivia)
  * [Comments and docstrings](#comments-and-docstrings)

<!-- tocstop -->

## Features

* Install Emacs packages from [MELPA], [GNU ELPA][gnu-elpa],
  [EmacsMirror], or manually specified sources.
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
* No support whatsoever for `package.el`.
* Edit packages by editing their code, no extra steps required. Allow
  for manual version control operations.
* Compatibility with MELPA, GNU ELPA, and EmacsMirror.
* Trivial to quickly try out a package without permanently installing
  it.
* Good for reproducing an issue with `emacs -Q`.

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

### TL;DR

`straight.el` operates by cloning Git repositories and then symlinking
files into Emacs' load path. The collection of symlinked files
constitutes the package, which is defined by its recipe. The recipe
also describes which local repository to link the files from, and how
to clone that repository, if it is absent.

When you call `straight-use-package`, the recipe you provide is
registered with `straight.el` for future reference. Then the package's
repository is cloned if it is absent, the package is rebuilt if its
files have changed since the last build (as determined by `find(1)`),
and its autoloads are evaluated.

You can also provide only a package name, in which case the recipe
will be looked up in one of several configurable recipe repositories,
which are just packages themselves (albeit with the build step
disabled).

`straight.el` determines which packages are installed solely by how
and when `straight-use-package` is invoked in your init-file, so some
optimizations and validation operations require you to provide
additional contextual information about the init process via
`straight-declare-init-succeeded` and
`straight-declare-init-finished`.

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
to `package.el`).

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

(Disclaimer: while I try to be as objective and comprehensive as
possible here, I'm obviously biased. Please submit corrections if I
have unfairly disparaged your favorite package manager!)

There are many package managers for Emacs, ranging from simple scripts
to download files from EmacsWiki to full-featured package management
solutions like `straight.el`. Here are the most feature-rich
alternatives to `straight.el`:

* [`package.el`][package.el]: de facto standard, bundled with Emacs.
* [Quelpa]: allows you to use external sources like GitHub with
  `package.el`. Essentially a local [MELPA].
* [Cask]: another `package.el` wrapper. Specify your dependencies in a `Cask` file; can be used for project management or an Emacs configuration.
* [el-get]: ridiculously OP in terms of how many different sources you
  can pull packages from (`package.el`, every known VCS, distro
  package managers, `go get`(!!)).
* [Borg]: assimilates packages as Git submodules into `.emacs.d`,
  relying on [EmacsMirror].
* "Screw package managers! I'll just handle it all myself!"

### TL;DR

Here is a summary of the main differences in philosophy between the
package managers:

* Use `package.el` if you want package management to be as easy as
  possible, and do not much care for installing packages from specific
  sources, keeping track of their versions, or doing local development
  on them.
* Use Quelpa if you like `package.el` but really wish you could
  specify the sources of your packages.
* Use Cask if you like `package.el` but wish it came with some project
  management tools, as well.
* Use el-get if you want to easily install packages from as many
  different sources as possible.
* Use Borg if you like a more lightweight approach to package
  management that leverages existing solutions, if contributing
  changes to packages upstream is important to you, and if using Git
  submodules isn't a deal-breaker.
* Use the manual approach if you need to contribute changes to a
  package that is versioned in something other than Git.
* Use `straight.el` if you like reproducibility in your Emacs
  configuration, you regularly contribute changes to packages
  upstream, and you think deferred installation is a really great
  idea.

And here is a brief list of the main reasons you might not want to use
`straight.el`:

* `straight.el` is largely unusable if you do not have Git installed,
  although it is still possible to use the package-building features
  if you manage your repositories manually (you also cannot use the
  magic bootstrap snippet, in that case). If you don't want to install
  Git, you'll have to use `package.el` or take the manual approach.
* `straight.el` is not built in to Emacs. If you want something that
  will work right out of the box, you're stuck with `package.el` or
  the manual approach.
* `straight.el` takes a minute or two to update all your packages,
  since it does not rely on a centralized server. If you want quick
  update checking, you'll have to use `package.el`.
* `straight.el` does not provide any user interface for package
  management. For that, you'll have to use `package.el`, el-get, Cask,
  or Borg (which expects you to use [`epkg`][epkg] for browsing
  packages).
* `straight.el` does not currently support using only stable versions
  of packages (i.e. tagged revisions), although this is
  a [planned feature][tag-only-issue]. If this is important to you,
  you probably want to go with `package.el` (with GNU ELPA and MELPA
  Stable), Cask, or Quelpa.
* `straight.el` does not currently support Texinfo, although this is
  a [planned feature][texinfo-issue]. Texinfo is supported by
  `package.el`, Quelpa, el-get, Cask, and Borg. You can also compile
  Texinfo manuals by hand.
* `straight.el` does not currently support arbitrary build commands
  like `make`, although this is
  a [planned feature][build-command-issue]. This feature is supported
  by el-get and Borg.
* If you don't like having to modify your init-file to do package
  management, then `straight.el` is absolutely not for you. You want
  `package.el`, Quelpa, el-get, or Borg.
* If you really want to contribute changes to packages that are not
  versioned in Git, then `straight.el` will not help you. You'll have
  to manage the package's repository manually. Unfortunately, there is
  no existing package manager that supports both non-Git
  version-control systems and contributing changes upstream. You'll
  have to go with the manual approach.
* `straight.el` does not provide project management tools. It is a
  package manager. If you want project management tools, check out
  Cask.
* `straight.el` is quite new and moving fast. Things might break. The
  other package managers can generally be ranked as follows, from most
  active to least active: el-get, Quelpa, Borg, Cask, `package.el`
  (glacial).

### Comparison to `package.el`

* `package.el` downloads pre-built packages from central servers using
  a special (undocumented?) HTTP protocol, while `straight.el` clones
  Git (or other) repositories and builds packages locally.

#### Advantages of `straight.el`

* `straight.el` allows you to install a package from any branch of any
  Git repository. `package.el` only allows you to install a package
  from a `package.el`-compliant central server.
* `straight.el` allows you to check out any Git revision of any
  package. `package.el` only allows you to install the latest version,
  and there is no way to downgrade.
* `straight.el` supports EmacsMirror, while `package.el` does not.
* `straight.el` uses your init-file as the sole source of truth for
  package operations. `package.el` loads every package you ever
  installed at startup, even if some of those packages are no longer
  referenced by your init-file.
* `straight.el` supports 100% reproducibility for your Emacs packages
  with version lockfiles. `package.el` cannot provide reproducibility
  for the set of packages installed, the central servers they were
  installed from, or the versions in use.
* `straight.el` allows you to make arbitrary changes to your packages
  locally. While it is possible to make local changes to `package.el`
  packages, these changes cannot be version-controlled and they will
  be silently overwritten whenever `package.el` performs an update.
* `straight.el` allows you to perform arbitrary version-control
  operations on your package's Git repositories, including
  contributing changes upstream. `straight.el` has explicit support
  for specifying both an upstream and a fork for a package.
  Contributing changes upstream with `package.el` is impossible.
* `straight.el` is designed with `emacs -Q` bug reports in mind.
  `package.el` is unsuitable for minimal bug reproductions, since it
  automatically loads all of your packages on any package operation,
  even in `emacs -Q`.
* `straight.el` operates quietly when all is going well. `package.el`
  displays all messages, errors, and warnings that come from
  byte-compilation and autoload generation.
* `straight.el` considers modifying the user's init-file extremely
  uncouth. `package.el` aggressively inserts a call to
  `package-initialize` into the init-file if it is not already
  present, whenever any package management operation is performed.
* `straight.el` is developed openly on GitHub, using a
  modern [issue tracker][issues] and continuous integration
  from [Travis CI][travis-build]. It welcomes contributions of any
  type. `straight.el` is licensed under the permissive MIT license and
  does not require a copyright assignment. `straight.el` is developed
  actively and has explicit support for installing development
  versions of itself, as well as for contributing upstream changes.
  `package.el` is maintained as a part of Emacs core, meaning that the
  contribution process is poorly documented and discouraging. Releases
  of `package.el` coincide with releases of Emacs, which are
  infrequent and inflexible. There is no issue tracker specifically
  for `package.el`, only the Emacs bug tracker and the emacs-devel
  mailing list. Contributing to `package.el` requires a
  poorly-documented, cumbersome copyright assignment process and is
  done by submitting patches to
  an [unfriendly][emacs-devel-unfriendly] mailing list, unsupported by
  modern code review tooling or continuous integration.

#### Advantages of `package.el`

* `package.el` does not require that you have Git installed, since the
  central server deals with where the packages originally came from.
  `straight.el` cannot be used at all without Git.
* `package.el` is built in to Emacs and does not require additional
  configuration to get started with. `straight.el` requires the
  use of a 10-line bootstrap snippet in your init-file.
* `package.el` can perform bulk package updates more quickly since it
  relies on central servers.
* `package.el` has a user interface for package management that also
  displays package metadata. `straight.el` has no user interface for
  package management; any UI is provided by the user's
  `completing-read` framework.
* `package.el` does not require you to touch your init-file to install
  packages, while `straight.el` absolutely refuses to permanently
  install a package without an explicit reference to it in your
  init-file (although this may be considered an advantage, depending
  on your perspective).
* Using MELPA Stable, `package.el` can install only stable versions of
  packages. By default, `package.el` also installs only stable
  versions of packages from GNU ELPA. These modes of operation are
  unsupported by `straight.el` at this time, although this is
  a [planned feature][tag-only-issue].

#### Additional notes

* `package.el` and `straight.el` usually take approximately the same
  time to install packages, despite the fact that `straight.el` is
  cloning entire Git repositories. This is because network latency and
  byte-compilation are the dominant factors.
* Some `package.el` servers build packages from non-Git upstreams.
  `package.el` can install these packages, while `straight.el` cannot.
  However, since `package.el` has no version-control support, this is
  more or less equivalent to installing those packages from the
  [EmacsMirror], which `straight.el` can do by default.

### Comparison to Quelpa

* Quelpa allows for fetching packages from arbitrary sources and
  building them into a format that can be installed by `package.el`.
  `straight.el` has a philosophy which is fundamentally incompatible
  with `package.el`, and non-compatibility with `package.el` is one of
  its design goals.

#### Advantages of `straight.el`

* `straight.el` has out-of-the-box compatibility with GNU ELPA and
  EmacsMirror, while Quelpa only has support for MELPA. To use GNU
  ELPA, you must drop down to `package.el`. [EmacsMirror] is not
  supported by default, although it is easy to specify an EmacsMirror
  repository in a recipe. While Quelpa allows you to specify custom
  recipe folders, it does not have support for cloning these folders
  automatically from version control, nor for generating the recipes
  in any way other than copying them literally from files.
  `straight.el` allows you full flexibility in this regard.
* `straight.el` has integrated support for selecting particular Git
  revisions of packages. This process is more manual in Quelpa, as it
  requires placing the commit hash into the recipe, which disables
  updates.
* `straight.el` uses your init-file as the sole source of truth for
  package operations. Since Quelpa is based on `package.el`, it also
  loads every package you ever installed at startup, even if those
  packages are no longer referenced by your init-file. Furthermore,
  there is an additional caching layer, so that deleting a package
  from the `package.el` interface and removing it from your init-file
  still does not actually delete it.
* `straight.el` supports 100% reproducibility for your Emacs packages
  with version lockfiles. Quelpa can theoretically provide some
  measure of reproducibility, but this requires significant manual
  effort since all packages are not associated with specific revisions
  by default, nor is the revision of MELPA saved anywhere.
* `straight.el` allows you to make arbitrary changes to your packages
  locally. While it is possible to make local changes to Quelpa
  packages, there are two places to do so: the built package, which is
  the default destination of `find-function`, and the original
  repository. Changes to the former are not version-controlled and
  will be silently overwritten by `package.el` operations, while
  changes to the latter will be silently overwritten by Quelpa
  operations.
* `straight.el` has explicit support for configuring both an upstream
  repository and a fork for the same package. Quelpa does not have
  such a concept.
* `straight.el` allows you complete control over how your repositories
  are managed, and the default behavior is to draw all packages
  versioned in a single repository from a single copy of that
  repository. Quelpa is hardcoded to require a separate repository for
  each package, so that installing Magit requires three copies of the
  Magit repository.
* `straight.el` builds packages using symlinks, meaning that
  `find-function` works as expected. Quelpa builds packages by
  copying, a feature inherited from MELPA. This means that
  `find-function` brings you to the built package, instead of the
  actual repository, which is not version-controlled and will be
  overwritten whenever `package.el` performs an update.
* `straight.el` allows you to perform arbitrary version-control
  operations on your package's Git repositories. Quelpa allows this,
  but all local changes will be silently overridden whenever Quelpa
  performs an update.
* `straight.el` is designed with `emacs -Q` bug reports in mind.
  Since Quelpa is based on `package.el`, it is also unsuitable for
  minimal bug reproductions, since it automatically loads all of your
  packages on any package operation, even in `emacs -Q`.
* `straight.el` operates quietly when all is going well. Since Quelpa
  is based on `package.el`, it displays all messages, errors, and
  warnings that come from byte-compilation and autoload generation. It
  also displays additional messages while cloning Git repositories,
  downloading files, and building packages from their repositories
  into `package.el` format.
* `straight.el` does not modify your init-file. Since Quelpa is based
  on `package.el`, it inherits the behavior of aggressively inserting
  a call to `package-initialize` into your init-file on any package
  management operation.

#### Advantages of Quelpa

* Quelpa supports all the version-control systems supported by MELPA,
  which is to say almost every commonly and uncommonly used VCS.
  `straight.el` only supports Git, although it is designed to support
  other version-control backends.
* Quelpa allows for installing only stable versions of packages, from
  any source. This mode of operation is unsupported by `straight.el`,
  although it is a [planned feature][tag-only-issue].
* Since Quelpa is based on `package.el`, it inherits a user interface
  for package management that also displays package metadata.
  `straight.el` has no such interface.

#### Additional notes

* `straight.el` and Quelpa both allow you to manage your package's
  local repositories manually, if you wish.
* In principle, `straight.el` and Quelpa have identical package
  installation times, since they are performing the same operations.
  In practice, Quelpa is slightly slower since it builds packages by
  copying rather than symlinking, and it clones multiple copies of the
  same Git repository when multiple packages are built from it.
* `straight.el` encourages you to keep a tight handle on your package
  versions by default. Quelpa encourages you to stick to the latest
  versions of your packages, and to upgrade them automatically.

### Comparison to Cask

I have not used Cask extensively, so please feel especially free to
offer corrections for this section.

* Cask installs packages using the `package.el` protocol, based on a
  `Cask` file written in the Cask DSL. `straight.el` eschews
  `package.el` entirely, and clones packages from source based on how
  you invoke `straight-use-package` in your init-file.
* Cask focuses more on being a build manager, like Maven or Leiningen,
  while `straight.el` focuses *exclusively* on being a package
  manager.

#### Advantages of `straight.el`

* `straight.el` has out-of-the-box compatibility with EmacsMirror,
  while Cask only supports `package.el`-compliant repositories.
  However, it is easy to specify an EmacsMirror repository in a
  recipe. Cask does not support custom package sources. `straight.el`
  supports GNU ELPA, MELPA, and EmacsMirror, and allows you to add any
  other sources you would like.
* `straight.el` has integrated support for selecting particular Git
  revisions of packages. This process is more manual in Cask, as it
  requires placing the commit hash into the recipe, which disables
  updates.
* `straight.el` uses your init-file as the sole source of truth for
  package operations. Since Cask is based on `package.el`, it loads
  every package you ever installed at startup, even if some of those
  packages are no longer referenced by your `Cask` file.
* `straight.el` determines your package management configuration
  implicitly by detecting how you call `straight-use-package` in your
  init-file and making the appropriate changes immediately. Cask
  requires manual intervention (for example, issuing a `cask install`
  command when you have updated your `Cask` file). However, both
  `straight.el` and Cask can be considered declarative package
  managers.
* `straight.el` supports 100% reproducibility for your Emacs packages
  with version lockfiles. Cask can theoretically provide some measure
  of reproducibility, but this requires significant manual effort
  since all packages are not associated with specific revisions by
  default, nor is the revision of Cask saved anywhere.
* `straight.el` allows you to make arbitrary changes to your packages
  locally. While it is possible to make local changes to Cask
  packages, these will not be version-controlled and they will be
  silently overwritten or shadowed when Cask performs an update.
* `straight.el` allows you to perform arbitrary version-control
  operations on your package's Git repositories, including
  contributing changes upstream. `straight.el` has explicit support
  for specifying both an upstream and a fork for a package.
  Contributing changes upstream with Cask is impossible.
* `straight.el` is designed with `emacs -Q` bug reports in mind. Cask
  appears to be unsuitable for minimal bug reproductions, since there
  does not appear to be a straightforward way to load a single
  package, without loading all other packages configured in your
  `Cask` file.
* `straight.el` operates quietly when all is going well. Since Cask is
  based on `package.el`, it displays all messages, errors, and
  warnings that come from byte-compilation and autoload generation.

#### Advantages of Cask

* Cask provides a useful toolbox of operations for project management,
  which are completely absent from `straight.el`.
* Since Cask is based on `package.el`, it does not require that you
  have Git installed. (It does require Python, however.) `straight.el`
  is mostly unusable without Git.
* Since Cask is based on `package.el`, it can perform bulk package
  updates more quickly than `straight.el`.
* Since Cask is based on `package.el`, it inherits a user interface
  for package management that also displays package metadata.
* Since Cask is based on `package.el`, you can install packages
  without editing a file manually, although this rather defeats the
  entire purpose of using Cask instead of `package.el`. `straight.el`
  absolutely refuses to permanently install a package without an
  explicit reference to it in your init-file (although this may be
  considered an advantage, depending on your perspective).
* Using MELPA Stable, Cask can install only stable versions of
  packages. By default, Cask also installs only stable versions of
  packages from GNU ELPA. These modes of operation are unsupported by
  `straight.el` at this time, although this is
  a [planned feature][tag-only-issue].
* Cask supports more version-control systems than `straight.el` (which
  only supports Git).

### Comparison to el-get

I have not used el-get extensively, so please feel especially free to
offer corrections for this section.

* Both el-get and `straight.el` implement their own package management
  abstractions instead of delegating to `package.el`. However:
    * el-get goes the route of adding as many package sources as
      possible (e.g. `package.el`, many different version-control
      systems, various specific websites, and even system package
      managers) so that packages can be used very easily
    * `straight.el` only supports Git and in doing so is able to
      provide more advanced package management features.

#### Advantages of `straight.el`

* `straight.el` has integrated support for selecting particular Git
  revisions of packages. This process is more manual in el-get, as it
  requires placing the commit hash into the recipe, which disables
  updates.
* `straight.el` uses your init-file as the sole source of truth for
  package operations. el-get has additional metadata stored outside
  the init-file, although specifying all packages in your init-file is
  a supported mode of operation.
* `straight.el` supports 100% reproducibility for your Emacs packages
  with version lockfiles. el-get can theoretically provide some
  measure of reproducibility, but this requires significant manual
  effort since all packages are not associated with specific revisions
  by default, nor is the revision of el-get saved anywhere.
* `straight.el` allows you to make arbitrary changes to your packages
  locally, and conflicts during updates are presented to the user and
  resolved interactively. While it is possible to make local changes
  to el-get packages, the el-get manual warns that such changes may
  break the update mechanism.
* `straight.el` has explicit support for configuring both an upstream
  repository and a fork for the same package. el-get does not have
  such a concept.
* `straight.el` allows you to perform arbitrary version-control
  operartions on your package's Git repositories. el-get allows this,
  but local changes will be overwritten when el-get performs an
  update.
* `straight.el` provides a suite of powerful interactive tools for
  performing bulk operations on your package's Git repositories.
  el-get only allows you to install, uninstall, and update packages
  noninteractively.
* `straight.el` operates quietly when all is going well. el-get
  reports its progress verbosely.

#### Advantages of el-get

* el-get supports virtually all known version-control systems, as well
  as system package managers, EmacsWiki, arbitrary HTTP, and even `go
  get`. `straight.el` supports only Git, although it does allow you to
  manage your local repositories manually if you would like.
* el-get has been around since 2010 and is on its fifth major version,
  whereas `straight.el` was created in January 2017 and is only now
  approaching a 1.0 release. Clearly, el-get is more stable, although
  despite its recency `straight.el` is already almost 50% of the size
  of el-get, by the line count. Both package managers are actively
  maintained.
* el-get has a recipe format which is several orders of magnitude more
  powerful than that of `straight.el`, since it supports many more
  package sources that can be configured and since it allows for a
  more complex build process.
* el-get supports texinfo, while `straight.el` does not yet (but this
  is a [planned feature][texinfo-issue]).
* el-get provides for running initialization code for packages.
  `straight.el` does not support this and expects you to use a
  dedicated tool like [use-package] (with which integration is built
  in) for that purpose.

### Comparison to Borg

* Borg and `straight.el` are perhaps the two most similar package
  managers on this list. The difference is that Borg is very minimal
  and expects you to complement it with other tools such as [Magit],
  [epkg], [use-package], and [auto-compile]. On ther other hand,
  `straight.el` aspires to be a one-stop package management solution,
  although it does not try to replace dedicated version-control
  packages (Magit) or dedicated package *configuration* packages
  (use-package).
* Borg uses Git submodules, while `straight.el` uses independently
  managed Git repositories.

#### Advantages of `straight.el`

* `straight.el` supports GNU ELPA, MELPA, EmacsMirror, and custom
  recipe sources. Borg only supports EmacsMirror.
* Borg, even when combined with related tools, do not allow for the
  kind of massive interactive repository management provided by
  `straight.el`.
* `straight.el` allows you to specify a custom recipe to install a
  package from any Git repository. It appears that achieving the same
  with Borg requires updating the EmacsMirror.
* `straight.el` supports deferred and conditional installation. This
  is not supported by Borg, although it could in principle be
  implemented via lazy cloning of submodules.
* `straight.el` provides an API designed for other version-control
  backends to be added in future. Borg is inextricably tied to Git.
* The interface for Git submodules has a number of sharp edges.
* `straight.el` provides dependency management. This is a manual
  process in Borg.
* `straight.el` provides mechanisms for updating your packages. This
  is a manual process in Borg.

#### Advantages of Borg

* Borg does a heck of a lot less magic, so if you want a solution with
  simple implementation details, `straight.el` may not be for you.
  (But see the developer manual and docstrings, first.)
* Borg supports texinfo and arbitrary build commands; `straight.el`
  does not (although these
  are [planned][texinfo-issue] [features][build-command-issue]).

### Comparison to the manual approach

* The manual approach is to download packages yourself and put them on
  your `load-path`. `straight.el` is more or less what you get when
  you take the manual approach, think very hard about the best way to
  do everything, and then automate all of it.

#### Advantages of `straight.el`

* `straight.el` figures out where to clone your packages from for you.
* `straight.el` byte-compiles your packages for you and generates
  their autoloads automatically.
* `straight.el` frees you from needing to manually recompile and
  regenerate autoloads.
* `straight.el` keeps track of dependencies for you.
* `straight.el` provides tools to manage all your packages in bulk,
  which would otherwise be a long, tedious process.
* `straight.el` allows you to get reproducibility for your
  configuration without needing to keep all of your packages under
  version control.
* `straight.el` (when used with [use-package]) automates the complex
  process of deferred installation.
* `straight.el` links packages into a separate build directories.
  Running packages directly from their repositories has a number of
  problems, including:
    * making it impossible to run only one package, if others are
      provided in the same repository.
    * making your working directory dirty when the package author
      forgot to add their build artifacts like `*.elc` and autoload
      files to the `.gitignore`.
    * instantly losing compatibility with MELPA recipes.
* `straight.el` offers you a single entry point to install only a
  single package in isolation, for a minimal bug reproduction. With
  the manual approach this would be more complicated, especially if
  the package has dependencies.
* `straight.el` frees you from needing to think about package
  management, since I already did all the thinking to figure how best
  to design everything.

#### Advantages of the manual approach

* No dependencies.
* You learn a lot, if you don't give up first.
* You might end up writing a package manager (case in point).
* This is the only way to deal with packages that have non-Git
  upstreams which you need to contribute changes to. (However, you can
  always use the manual approach for one package and `straight.el` for
  the rest. Or you can just eschew `straight.el`'s version-control
  support for that package, and use it only for building the package.)

## User manual

FIXME

## Developer manual

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

[auto-compile]: https://github.com/tarsius/auto-compile
[borg]: https://github.com/emacscollective/borg
[build-command-issue]: https://github.com/raxod502/straight.el/issues/72
[cask]: https://github.com/cask/cask
[el-get]: https://github.com/dimitri/el-get
[emacs-devel-unfriendly]: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=19296#14
[emacs]: https://www.gnu.org/software/emacs/
[emacsmirror]: https://emacsmirror.net/
[emacswiki]: https://www.emacswiki.org/
[epkg]: https://github.com/emacscollective/epkg
[gnu-elpa]: https://elpa.gnu.org/
[issues]: https://github.com/raxod502/straight.el/issues
[magit]: https://magit.vc/
[melpa-recipe-format]: https://github.com/melpa/melpa#recipe-format
[melpa]: http://melpa.org/#/
[package.el]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html
[quelpa]: https://github.com/quelpa/quelpa
[tag-only-issue]: https://github.com/raxod502/straight.el/issues/31
[texinfo-issue]: https://github.com/raxod502/straight.el/issues/71
[travis-build]: https://travis-ci.org/raxod502/straight.el
[use-package]: https://github.com/jwiegley/use-package

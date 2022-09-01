**straight.el**: next-generation, purely functional package manager
for the [Emacs] hacker.

**Please check out the [FAQ][#faq] and [news][#news] :)**

[![Gitter chat][gitter-badge]][gitter]

<!-- longlines-start -->

<!-- toc -->

- [Features](#features)
- [Guiding principles](#guiding-principles)
- [Getting started](#getting-started)
    + [Debugging](#debugging)
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
  * [Comparison to Cask](#comparison-to-cask)
    + [Advantages of `straight.el`](#advantages-of-straightel-2)
    + [Advantages of Cask](#advantages-of-cask)
  * [Comparison to el-get](#comparison-to-el-get)
    + [Advantages of `straight.el`](#advantages-of-straightel-3)
    + [Advantages of el-get](#advantages-of-el-get)
  * [Comparison to Borg](#comparison-to-borg)
    + [Advantages of `straight.el`](#advantages-of-straightel-4)
    + [Advantages of Borg](#advantages-of-borg)
  * [Comparison to the manual approach](#comparison-to-the-manual-approach)
    + [Advantages of `straight.el`](#advantages-of-straightel-5)
    + [Advantages of the manual approach](#advantages-of-the-manual-approach)
- [User manual](#user-manual)
  * [Bootstrapping `straight.el`](#bootstrapping-straightel)
  * [Installing packages programmatically](#installing-packages-programmatically)
    + [Installing with a custom recipe](#installing-with-a-custom-recipe)
    + [Additional arguments to `straight-use-package`](#additional-arguments-to-straight-use-package)
    + [Variants of `straight-use-package`](#variants-of-straight-use-package)
    + [Customizing when packages are built](#customizing-when-packages-are-built)
      - [Custom or manual modification detection](#custom-or-manual-modification-detection)
      - [Summary of options for package modification detection](#summary-of-options-for-package-modification-detection)
        * [`find-at-startup`](#find-at-startup)
        * [`check-on-save`](#check-on-save)
        * [`watch-files`](#watch-files)
    + [Customizing how packages are built](#customizing-how-packages-are-built)
      - [Autoload generation](#autoload-generation)
      - [Byte compilation](#byte-compilation)
      - [Native compilation](#native-compilation)
      - [Symbolic links](#symbolic-links)
    + [Customizing how packages are made available](#customizing-how-packages-are-made-available)
    + [Hooks run by `straight-use-package`](#hooks-run-by-straight-use-package)
  * [The recipe format](#the-recipe-format)
    + [Version-control backends](#version-control-backends)
    + [Git backend](#git-backend)
      - [Deprecated `:upstream` keyword](#deprecated-upstream-keyword)
  * [Recipe lookup](#recipe-lookup)
    + [Updating recipe repositories](#updating-recipe-repositories)
    + [Customizing recipe repositories](#customizing-recipe-repositories)
      - [GNU ELPA](#gnu-elpa)
      - [Emacsmirror](#emacsmirror)
      - [Defining new recipe repositories](#defining-new-recipe-repositories)
  * [Overriding recipes](#overriding-recipes)
    + [Overriding the recipe for `straight.el`](#overriding-the-recipe-for-straightel)
  * [Interactive usage](#interactive-usage)
    + [Version control operations](#version-control-operations)
  * [Lockfile management](#lockfile-management)
    + [The profile system](#the-profile-system)
  * [Packages and the init-file](#packages-and-the-init-file)
  * [Using `straight.el` to reproduce bugs](#using-straightel-to-reproduce-bugs)
    + [... in other packages](#-in-other-packages)
    + [... in `straight.el` itself](#-in-straightel-itself)
  * [Using `straight.el` to develop packages](#using-straightel-to-develop-packages)
  * [Integration with other packages](#integration-with-other-packages)
    + [Integration with `use-package`](#integration-with-use-package-1)
    + ["Integration" with `package.el`](#integration-with-packageel)
    + [Integration with Flycheck](#integration-with-flycheck)
    + [Integration with Hydra](#integration-with-hydra)
  * [Miscellaneous](#miscellaneous)
- [Developer manual](#developer-manual)
  * [Low-level functions](#low-level-functions)
- [Trivia](#trivia)
  * [Comments and docstrings](#comments-and-docstrings)
- [Contributing](#contributing)
- [FAQ](#faq)
  * [My init time got slower](#my-init-time-got-slower)
  * ["Could not find package in recipe repositories"](#could-not-find-package-in-recipe-repositories)
  * [How do I update MELPA et al.?](#how-do-i-update-melpa-et-al)
  * [My `use-package` form isn't working properly](#my-use-package-form-isnt-working-properly)
  * [How do I uninstall a package?](#how-do-i-uninstall-a-package)
  * [The wrong version of my package was loaded](#the-wrong-version-of-my-package-was-loaded)
  * [I get "could not read username/password" errors](#i-get-could-not-read-usernamepassword-errors)
  * [How do I pin package versions or use only tagged releases?](#how-do-i-pin-package-versions-or-use-only-tagged-releases)
  * [How can I use the built-in version of a package?](#how-can-i-use-the-built-in-version-of-a-package)
- [News](#news)
  * [Jan 1, 2021](#jan-1-2021)
  * [April 19, 2020](#april-19-2020)

<!-- tocstop -->

<!-- longlines-stop -->

## Features

* Install Emacs packages listed on [MELPA], [GNU ELPA][gnu-elpa], or
  [Emacsmirror], or provide your own recipes.
* Packages are cloned as Git (or other) repositories, not as opaque
  tarballs.
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
* Specify package descriptions using a powerful format based on [MELPA
  recipes][melpa-recipe-format] (with a familiar but improved syntax).
* [`use-package`][use-package] integration.
* Modular: you can install your packages manually and straight.el will
  load them for you. Or you can also have straight.el install your
  packages, while you provide the recipes explicitly. Or straight.el
  can also fetch recipes, if you want. Bulk repository management and
  package updates are also optional.
* Extensible APIs to add new recipe sources and version-control
  backends.
* The cleanest source code you've ever seen. [45%][#trivia/comments]
  of `straight.el` is comments and docstrings.

Note: `straight.el` is a replacement for `package.el`, **not**
`use-package`. `use-package` can be used with either `package.el` or
`straight.el`.

## Guiding principles

* Init-file and version lockfiles as the sole source of truth. No
  persistent state kept elsewhere.
* 100% reproducible package management, accounting for changes in
  packages, recipe repositories, configuration, and the package
  manager itself.
* No support whatsoever for `package.el`.
* Edit packages by editing their code, no extra steps required. Allow
  for manual version control operations.
* Compatibility with MELPA, GNU ELPA, and Emacsmirror.
* Trivial to quickly try out a package without permanently installing
  it.
* Good for reproducing an issue with `emacs -Q`.

## Getting started

> **Note: `straight.el` supports a minimum version of Emacs 25.1, and
> works on macOS, Windows, and most flavors of Linux. You must install
> [Git] in order to use `straight.el`.**

First, place the following bootstrap code in your init-file:

<!-- longlines-start -->

```emacs-lisp
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
```

<!-- longlines-stop -->

Here are some variables you may be interested in (some of them must be
set **before** the bootstrap code runs, if they might affect how
`straight.el` itself is loaded):

* [`straight-repository-branch`][#user/overriding/straight.el] -- to
  get the latest version of `straight.el` from the `develop` branch,
  rather than the default `master` which is updated less frequently
  but which is ostensibly more stable.
* [`straight-check-for-modifications`][#user/install/mod-detection] --
  to configure an alternate way for `straight.el` to check for
  modifications made to package source code, rather than the default
  (which is 100% reliable, but has a minor cost to startup time).
* [`straight-use-package-by-default`][#user/integration/use-package]
  -- if you use [`use-package`][use-package], then this makes each
  `use-package` form also invoke `straight.el` to install the package,
  unless otherwise specified.
* [`straight-vc-git-default-protocol`][#user/recipes/git] -- by
  default, `straight.el` clones over HTTPS. If you need packages from
  private Git repositories in your configuration, then you might want
  to use SSH instead.
* `straight-base-dir` --
  by default, straight's main directory, containing it's build files and
  package repos, is located in `user-emacs-directory`. You can change
  the location of straight's base directory via this variable.
* `straight-build-dir` --
  by default, the directory in which packages are built is located at
  `straight-base-dir`/build. Changing this variable will change the name
  of that directory and the name of the build cache file (unless
  `straight-build-cache-fixed-name` is non-nil).

You should remove any code that relates to `package.el`; for example,
references to `package-initialize`, `package-archives`, and (if you're
using [`use-package`][use-package]) `:ensure` or
`use-package-always-ensure`.

Users of Emacs versions >= 27 will want to add:

```emacs-lisp
(setq package-enable-at-startup nil)
```

to their [early init-file][early-init-file] to prevent package.el
loading packages prior to their init-file loading.

While it is technically possible to use
both `package.el` and `straight.el` at the same time, there is no real
reason to, and it might result in oddities like packages getting
loaded more than once.

#### Debugging

* Sometimes, in a corporate environment, `url-retrieve-synchronously`
  may not work and `straight.el` will be unable to download the
  installation script mentioned in the bootstrap snippet. In this
  case, you may simply clone this repository into
  `~/.emacs.d/straight/repos/straight.el` and check out your desired
  revision/branch. The installation script is just a more convenient
  way of doing that automatically when necessary (and looking up the
  correct revision of `straight.el` in your lockfile, if any).
* On macOS, you may receive an error:

      Could not create connection to raw.githubusercontent.com:443

  This is likely because you are using an ancient version of Emacs
  which has a broken TLS configuration. Upgrade with `brew upgrade
  emacs`.

### Install packages

Out of the box, you can install any package listed on [MELPA], [GNU
ELPA][gnu-elpa], or [Emacsmirror], which is to say any package in
existence. (Although MELPA is used as a package listing, packages are
installed by cloning their Git repositories rather than by downloading
tarballs like `package.el` does.) To install a package temporarily
(until you restart Emacs), run `M-x straight-use-package` and select
the package you want. To install a package permanently, place a call
to `straight-use-package` in your init-file, like:

```emacs-lisp
(straight-use-package 'el-patch)
```

Note that installing a package will activate all of its autoloads, but
it will not actually `require` the features provided by the package.
This means that you might need to use `require` or `autoload` for some
antiquated packages that do not properly declare their autoloads.

To learn more, see the documentation on [the package
lifecycle][#user/install].

### But what about my fork of (obscure .el package)?

Instead of passing just a package name to `straight-use-package`, you
can pass a list ("recipe"). You can see the default recipe for any
given package by running `M-x straight-get-recipe`. For example, the
recipe for `el-patch` is:

```emacs-lisp
(el-patch :type git :host github :repo "radian-software/el-patch")
```

So, if you have forked `el-patch` and you want to use your fork
instead of the upstream, do:

```emacs-lisp
(straight-use-package
 '(el-patch :type git :host github :repo "your-name/el-patch"))
```

In fact, `straight.el` has explicit support for using a forked
package, since this is so common:

```emacs-lisp
(straight-use-package
 '(el-patch :type git :host github :repo "radian-software/el-patch"
            :fork (:host github
                   :repo "your-name/el-patch")))
```

In the above, `:type git` may be omitted if you leave
`straight-default-vc` at its default value of `git`. Parts of the
`:fork` keyword may be omitted as well. One common case is when
your fork is on the same host and has the same name as the upstream
repository. In this case, assuming `straight-host-usernames` is set,
specifying a fork is as simple as:

```emacs-lisp
(straight-use-package
 '(el-patch :type git :host github :repo "radian-software/el-patch"
            :fork t))
```

Note that `straight.el` doesn't do any Git operations during startup
unless it needs to clone a package from scratch. This is for
performance. You can explicitly request for `straight.el` to fix up
the Git configuration after you change a package recipe, e.g. to add a
fork. See [Automatic repository management][#quickstart/vc] below.

To learn more, see the documentation on [the recipe
format][#user/recipes] and [the Git backend][#user/recipes/git].

### Integration with `use-package`

[`use-package`][use-package] is a macro that provides convenient
syntactic sugar for many common tasks related to installing and
configuring Emacs packages. Of course, it does not actually install
the packages, but instead defers to a package manager, like
`straight.el` (which comes with `use-package` integration by default).

To use `use-package`, first install it with `straight.el`:

```emacs-lisp
(straight-use-package 'use-package)
```

Now `use-package` will use `straight.el` to automatically install
missing packages if you provide `:straight t`:

```emacs-lisp
(use-package el-patch
  :straight t)
```

You can still provide a custom recipe for the package:

```emacs-lisp
(use-package el-patch
  :straight (el-patch :type git :host github :repo "radian-software/el-patch"
                      :fork (:host github
                             :repo "your-name/el-patch")))
```

The `:straight` keyword accepts backquoted forms.
This makes it possible to dynamically compute part of the recipe:

```emacs-lisp
(use-package el-patch
  :straight `(el-patch :type git
                       :repo ,(alist-get 'el-patch my-package-urls)))
```

Specifying `:straight t` is unnecessary if you set
`straight-use-package-by-default` to a non-nil value. (Note that the
variable `use-package-always-ensure` is associated with `package.el`,
and you should not use it with `straight.el`.)

To learn more, see the documentation on [`straight.el`'s `use-package`
integration][#user/integration/use-package].

### Edit packages locally

One of the biggest strengths of `straight.el` is that editing packages
locally is trivial. You literally just edit the files (`find-function`
and friends all work as you would expect). Packages will be
automatically rebuilt if necessary when Emacs next starts up.

You can even commit your changes and push or pull to various remotes
using Git. You have complete control over your packages' Git
repositories.

To learn more, see the documentation on [the package
lifecycle][#user/install].

### Automatic repository management

While being able to make arbitrary changes to your packages is very
powerful, it can also get tiring to keep track of the all those
changes. For this reason, `straight.el` provides a suite of powerful
interactive workflows to perform bulk operations on your packages.

* To restore each package to its canonical state (a clean working
  directory with the main branch checked out, and the remotes set
  correctly), run `M-x straight-normalize-package` or `M-x
  straight-normalize-all`.

* To fetch from each package's configured remote, run `M-x
  straight-fetch-package-and-deps` or `M-x straight-fetch-all`; to
  also fetch from the upstream for forked packages, supply a prefix
  argument.

* To merge changes from each package's configured remote, run `M-x
  straight-merge-package-and-deps` or `M-x straight-merge-all`; to
  also merge from the upstream for forked packages, supply a prefix
  argument.

* To push all local changes to each package's configured remote, run
  `M-x straight-push-package` or `M-x straight-push-all`.

All of these commands are highly interactive and ask you before making
any changes. At any point, you can stop and perform manual operations
with Magit or other tools in a recursive edit.

To learn more, see the documentation on [bulk repository
management][#user/interactive/vc].

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

To learn more, see the documentation on [version
lockfiles][#user/lockfiles].

## Conceptual overview

This section describes, at a high level, how the different mechanisms
in `straight.el` play together. This illustrates how `straight.el`
manages to accomplish all of its [guiding principles][#principles].

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
and when `straight-use-package` is invoked in your init-file.

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
`:files`, `:local-repo`, and `:build` keywords.

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

If `:build nil` is specified, however, this entire process is skipped.
This mechanism is used for recipe repositories.

### What does this look like on disk?

The local repositories are kept in `~/.emacs.d/straight/repos`, and
the built packages are kept in `~/.emacs.d/straight/build`. If you
have initialized `straight.el` and loaded package `el-patch`, then
your `~/.emacs.d/straight` directory will look roughly like this (some
irrelevant details have been omitted for pedagogical purposes):

<!-- longlines-start -->

```
straight
├── build
│   ├── el-patch
│   │   ├── el-patch-autoloads.el
│   │   ├── el-patch.el -> ~/.emacs.d/straight/repos/el-patch/el-patch.el
│   │   └── el-patch.elc
│   └── straight
│       ├── straight-autoloads.el
│       ├── straight.el -> ~/.emacs.d/straight/repos/straight.el/straight.el
│       └── straight.elc
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
```
<!-- longlines-stop -->

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
* `:fork`
* `:protocol`

If a local repository is not present, then its fetch recipe describes
how to obtain it. This is done using the `straight-vc-clone` function,
which delegates to one of the backend implementations of the `clone`
operation, according to `:type`. (The option `:type built-in` is a
special case that results in all version-control operations for the
package being ignored. You can also use `:type nil` to accomplish the
same, but with the difference that the package is still loaded from
its specified `:local-repo`.)

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
`straight-vc-get-commit` and `straight-vc-check-out-commit`.

The fetch recipe and version lockfiles, together with the
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
  `:build nil`, since recipe repositories usually do not need to be
  built)
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
registered, and not cloned until it is needed; see [the section on
`straight-use-package`][#concepts/straight-use-package]).

If you give `straight-use-package` just a package name, then each
recipe repository in `straight-recipe-repositories` is checked for a
recipe for that package. Once one is found, it is used. Otherwise, an
error is signaled (unless the package is built-in to Emacs, according
to `package.el`).

Note that `straight.el` uses its own recipe format which is similar,
but not identical, to the one used by MELPA (see [the section on the
recipe format][#user/recipes] for information on the differences). The
recipe repository backends abstract over the formatting differences in
different recipe sources to translate recipes into the uniform format
used by `straight.el`. When you run `M-x straight-get-recipe`, the
translated recipe is what is returned.

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
using `M-x straight-use-package` (or even by evaluating
`straight-use-package` forms). However, `straight.el` still provides
advanced features such as bulk package management and version locking.
This creates some interesting challenges which other package managers
do not have to deal with.

`straight.el` solves these problems using a concept called
*transactions*. The operation of the transaction system is mostly
transparent to the user, at least in recent versions of `straight.el`.
Basically, it provides a way for `straight.el` to keep track of what
happens within a single user operation (e.g. evaluate a buffer of
`straight-use-package` calls, or load the init-file).

`straight.el` uses the transaction system to keep track of what
packages you request in your init-file. If you invoke
`straight-use-package` interactively, then this invalidates that
information, since you have now requested a package that is not in
your init-file. For this reason, if you have invoked
`straight-use-package` interactively, running `M-x
straight-freeze-versions` will prompt you to first reload your
init-file.

**Note: reloading your init-file must have the effect of running all
of the same `straight.el`-related functions again. For example, if you
bootstrap `straight.el` in a sub-file that you only `require` instead
of `load`, then the reloading functionality will not work correctly
and you may receive the message `Caches are still outdated; something
is seriously wrong`. See [#437] for discussion.**

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
* [Cask]: another `package.el` wrapper. Specify your dependencies in a
  `Cask` file; can be used for project management or an Emacs
  configuration.
* [el-get]: ridiculously OP in terms of how many different sources you
  can pull packages from (`package.el`, every known VCS, distro
  package managers, `go get`(!!)).
* [Borg]: assimilates packages as Git submodules into `.emacs.d`,
  relying on [Emacsmirror].
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
  upstream, or you are writing an Emacs configuration to be used by
  others.

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
  of packages (i.e. tagged revisions), although this is a [planned
  feature][#31]. If this is important to you, you probably want to go
  with `package.el` (with GNU ELPA and MELPA Stable), Cask, or Quelpa.
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
* `straight.el` supports Emacsmirror, while `package.el` does not.
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
  uncouth. `package.el` aggressively inserts (via Customize)
  auto-generated code setting `package-selected-packages` into the
  init-file whenever a package is installed. Furthermore, `package.el`
  has a history of impolite treatment of user configuration, which I
  think says some things about the design: until Emacs 27.1 (when [my
  patch to fix this issue][early-init-file-commit] was adopted after
  around 300 emails' worth of squabbling on emacs-devel), it also
  inserted a call to the `package-initialize` function into the
  init-file if it was not already present, with the officially
  recommended workaround "comment it out if you don't want it, but
  don't get rid of it".
* `straight.el` has a profile system that allows users of someone
  else's Emacs configuration to manage an additional subset of
  packages, or to override upstream package configuration, without
  forking the upstream. `package.el` has no such concept.
* `straight.el` is developed openly on GitHub, using a modern [issue
  tracker][issues] and continuous integration from GitHub Actions. It
  welcomes contributions of any type. `straight.el` is licensed under
  the permissive MIT license and does not require a copyright
  assignment. `straight.el` is developed actively and has explicit
  support for installing development versions of itself, as well as
  for contributing upstream changes. `package.el` is maintained as a
  part of Emacs core, meaning that the contribution process is poorly
  documented and discouraging. Releases of `package.el` coincide with
  releases of Emacs, which are infrequent and inflexible. There is no
  issue tracker specifically for `package.el`, only the Emacs bug
  tracker and the emacs-devel mailing list. Contributing to
  `package.el` requires a poorly-documented, cumbersome copyright
  assignment process and is done by submitting patches to an
  antiquated mailing list, unsupported by modern code review tooling
  or continuous integration.

#### Advantages of `package.el`

* `package.el` does not require that you have Git installed, since the
  central server deals with where the packages originally came from.
  `straight.el` cannot be used at all without Git.
* `package.el` is built in to Emacs and does not require additional
  configuration to get started with. `straight.el` requires the use of
  a 10-line bootstrap snippet in your init-file.
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
  unsupported by `straight.el` at this time, although this is a
  [planned feature][#31].

#### Additional notes

* `package.el` and `straight.el` usually take approximately the same
  time to install packages, despite the fact that `straight.el` is
  cloning entire Git repositories. This is because network latency and
  byte-compilation are the dominant factors.
* Some `package.el` servers build packages from non-Git upstreams.
  `package.el` can install these packages, while `straight.el` cannot.
  However, since `package.el` has no version-control support, this is
  more or less equivalent to installing those packages from the
  [Emacsmirror], which `straight.el` can do by default.

### Comparison to Quelpa

* Quelpa allows for fetching packages from arbitrary sources and
  building them into a format that can be installed by `package.el`.
  `straight.el` has a philosophy which is fundamentally incompatible
  with `package.el`, and non-compatibility with `package.el` is one of
  its design goals.

#### Advantages of `straight.el`

* `straight.el` has out-of-the-box compatibility with MELPA, GNU ELPA,
  and Emacsmirror, while Quelpa only has support for MELPA. To use GNU
  ELPA, you must drop down to `package.el`. [Emacsmirror] is not
  supported by default, although it is easy to specify an Emacsmirror
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
* `straight.el` is designed with `emacs -Q` bug reports in mind. Since
  Quelpa is based on `package.el`, it is also unsuitable for minimal
  bug reproductions, since it automatically loads all of your packages
  on any package operation, even in `emacs -Q`.
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
* `straight.el` has a profile system that allows users of someone
  else's Emacs configuration to manage an additional subset of
  packages, or to override upstream package configuration, without
  forking the upstream. Quelpa has no such concept.

#### Advantages of Quelpa

* Quelpa supports all the version-control systems supported by MELPA,
  which is to say almost every commonly and uncommonly used VCS.
  `straight.el` only supports Git, although it is designed to support
  other version-control backends.
* Quelpa allows for installing only stable versions of packages, from
  any source. This mode of operation is unsupported by `straight.el`,
  although it is a [planned feature][#31].
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

* `straight.el` has out-of-the-box compatibility with Emacsmirror,
  while Cask only supports `package.el`-compliant repositories.
  However, it is easy to specify an Emacsmirror repository in a
  recipe. Cask does not support custom package sources. `straight.el`
  supports MELPA, GNU ELPA, and Emacsmirror, and allows you to add any
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
* `straight.el` has a profile system that allows users of someone
  else's Emacs configuration to manage an additional subset of
  packages, or to override upstream package configuration, without
  forking the upstream. Cask has no such concept.

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
  `straight.el` at this time, although this is a [planned
  feature][#31].
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
      managers) so that packages can be used very easily.
    * `straight.el` only supports Git and in doing so is able to
      provide more advanced package management features.

#### Advantages of `straight.el`

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
* `straight.el` provides a suite of powerful interactive workflows for
  performing bulk operations on your package's Git repositories.
  el-get only allows you to install, uninstall, and update packages.
* `straight.el` operates quietly when all is going well. el-get
  reports its progress verbosely.
* `straight.el` has a profile system that allows users of someone
  else's Emacs configuration to manage an additional subset of
  packages, or to override upstream package configuration, without
  forking the upstream. el-get has no such concept.

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
* el-get provides a number of features for running per-package
  initialization and setup code, including pulling that code from
  arbitrary sources. `straight.el` does not support this and expects
  you to use a dedicated tool like [`use-package`][use-package] (with
  which integration is built in) for that purpose.
* el-get has a user interface for package management that also
  displays package metadata, similarly to `package.el`. `straight.el`
  has no user interface for package management; any UI is provided by
  the user's `completing-read` framework.

### Comparison to Borg

* Borg and `straight.el` are perhaps the two most similar package
  managers on this list. The difference is that Borg is very minimal
  and expects you to complement it with other tools such as [Magit],
  [epkg], [`use-package`][use-package], and [auto-compile]. On the
  other hand, `straight.el` aspires to be a one-stop package
  management solution, although it does not try to replace dedicated
  version-control packages (Magit) or dedicated package
  *configuration* packages (`use-package`).
* Borg uses Git submodules, while `straight.el` uses independently
  managed Git repositories.

#### Advantages of `straight.el`

* `straight.el` supports MELPA, GNU ELPA, Emacsmirror, and custom
  recipe sources. Borg only supports Emacsmirror and custom recipe
  sources. However, as the Emacsmirror is a near-complete superset of
  both GNU ELPA and MELPA, this does not necessarily mean you have
  access to more packages: it just means you benefit from the recipe
  maintenance efforts of the MELPA team and the Emacsmirror team,
  rather than only the latter.
* Borg, even when combined with related tools, do not allow for the
  kind of massive interactive repository management provided by
  `straight.el`.
* `straight.el` provides an API designed for other version-control
  backends to be added in future. Borg is inextricably tied to Git.
* The interface for Git submodules has a number of sharp edges.
* `straight.el` provides dependency management. This is a manual
  process in Borg.
* `straight.el` provides mechanisms for updating your packages. This
  is a manual process in Borg.
* `straight.el` is configured solely by how you use in your init-file.
  Configuring Borg requires customizing `~/.emacs.d/.gitmodules`,
  which means (for example) that you cannot generate recipes
  dynamically. (However, the handling of configuration is
  [planned][#95-c1] to be improved in a future release.)
* `straight.el` has a profile system that allows users of someone
  else's Emacs configuration to manage an additional subset of
  packages, or to override upstream package configuration, without
  forking the upstream. Borg has no such concept.

#### Advantages of Borg

* Borg does a heck of a lot less magic, so if you want a solution with
  simple implementation details, `straight.el` may not be for you.
  (But see the developer manual and docstrings, first.)

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

This section tells you everything you need to know about the
user-facing features of `straight.el`. For implementation details, see
the [developer manual][#dev]. It may also be helpful to get some
perspective on the overarching concepts of `straight.el` from the
[conceptual overview][#concepts].

### Bootstrapping `straight.el`

In order to use `straight.el`, you will need to somehow get it loaded
into Emacs. (This is easy for `package.el`, since `package.el` is
built in to Emacs. `straight.el` must work a little harder.)

`straight.el` comes with a file to do just this, `bootstrap.el`. All
you need to do is load that file. You can do this with `M-x load-file`
or by a call to `load` in your init-file. However, there is an obvious
shortcoming: `bootstrap.el` will only be available once `straight.el`
is already installed.

You could just invoke `git clone` from your init-file, if
`straight.el` is not installed, but then you would have to manually
take care of selecting the correct branch, parsing your version
lockfile to check out the right revision, and so on. Instead, you can
just use this snippet, which uses a copious amount of magic to take
care of all these details for you:

<!-- longlines-start -->

```emacs-lisp
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
```

<!-- longlines-stop -->

Despite the reference to `develop`, this snippet actually installs
from the `master` branch by default, just like every other package.
Furthermore, the correct revision of `straight.el` is checked out, if
you have one specified in your lockfile. Even better, you can
[override the recipe for `straight.el`][#user/overriding/straight.el],
just like for any other package.

### Installing packages programmatically

The primary entry point to `straight.el` is the `straight-use-package`
function. It can be invoked interactively (for installing a package
temporarily) or programmatically (for installing a package
permanently). This section covers the programmatic usage; see
[later][#user/interactive] for interactive usage.

Here is the basic usage of `straight-use-package`:

```emacs-lisp
(straight-use-package 'el-patch)
```

This will ensure that the package `el-patch` is installed and loaded.
(Note that `straight-use-package` takes a symbol, not a string, for
the name of the package.) Precisely, this is what happens:

* If the local Git repository for `el-patch` is not available, it is
  cloned, and the appropriate revision is checked out (if one is
  specified in your version lockfiles).
* If the local Git repository has been modified since the last time
  the package was built, it is rebuilt. This means:
    * The `.el` files are symlinked into a separate directory to
      isolate them from other, irrelevant files.
    * The main package file is checked for dependencies, which are
      installed recursively if necessary using `straight-use-package`.
    * The `.el` files are byte-compiled.
    * Autoloads are extracted from the `.el` files and saved into a
      separate file.
* The package's directory is added to Emacs' `load-path`.
* The package's autoloads are evaluated.

Package authors should note that `straight.el` checks for dependencies
that are specified in the [`package.el` format][package.el-format]. To
spare you reading that documentation, this is either a
`Package-Requires` header in `PACKAGENAME.el`, or an argument to a
`define-package` invocation in `PACKAGENAME-pkg.el`. Despite the [many
shortcomings][#comparison/package.el/+straight.el] of `package.el`, it
has done a good job of creating a standardized format for dependency
declarations.

There is one exception to the above statement: not all entries
specified in the `Package-Requires` header necessarily correspond to
packages. For example, specifying a minimum Emacs version for a
package is done by depending on the `emacs` pseudo-package. Such
packages are simply ignored by `straight.el`, using the variable
`straight-built-in-pseudo-packages`.

Note that loading a package does not entail invoking `require` on any
of its features. If you wish to actually load the files of the
package, you need to do this separately. This is because most packages
do not need to be loaded immediately, and are better served by the
autoload system.

#### Installing with a custom recipe

`straight-use-package` can also take a list instead of a symbol. In
that case, the first member of the list is a symbol giving the package
name, and the remainder of the list is a [property
list][property-lists] providing information about how to install and
build the package. Here is an example:

```emacs-lisp
(straight-use-package
 '(el-patch :type git :host github :repo "radian-software/el-patch"
            :fork (:host github
                   :repo "your-name/el-patch")))
```

If you give `straight-use-package` just a package name, then a recipe
will be looked up by default (see the section on [recipe
lookup][#user/lookup]). You can see the default recipe for a package
by invoking [`M-x straight-get-recipe`][#user/interactive].

If `straight-allow-recipe-inheritance` is non-nil, then you only need
to specify the components of the recipe that you want to override. All
other components will still be looked up in the default recipe. In the
example above, we are only interested in changing the `:fork`
component. Therefore if `straight-allow-recipe-inheritance` is set,
the recipe could be simplifed as follows:

```emacs-lisp
(straight-use-package
 '(el-patch :fork (:repo "your-name/el-patch")))
```

or even simpler:

```emacs-lisp
(straight-use-package
 '(el-patch :fork "your-name/el-patch"))
```

The `:files` keyword and all version control keywords support
inheritance.

To learn more, see the section on [the recipe format][#user/recipes].

#### Additional arguments to `straight-use-package`

The full user-facing signature of `straight-use-package` is:

```emacs-lisp
(straight-use-package PACKAGE-OR-RECIPE &optional NO-CLONE NO-BUILD)
```

As discussed [previously][#user/install], by default
`straight-use-package` will do three things:

* Register the recipe provided with `straight.el`.
* Clone the package's local repository, if it is absent.
* Rebuild the package if necessary, and load it.

By providing the optional arguments, you may cause processing to halt
before all three of these tasks are completed. Specifically, providing
`NO-CLONE` causes processing to halt after registration but before
cloning, and providing `NO-BUILD` causes processing to halt after
cloning (if necessary) but before building and loading.

`straight.el` supports lazy-loading by means of a special value for
`NO-CLONE`, the symbol `lazy`. If this symbol is passed, then
processing will halt at the clone step, unless the package is already
cloned. This means that the package is built and loaded if it is
already installed, but otherwise installation is deferred until later.
When you want to trigger the lazy installation, simply call
`straight-use-package` again, but without `NO-CLONE`. (There is no
need to pass the recipe again; see [recipe lookup][#user/lookup].)

You can also pass functions for `NO-CLONE` or `NO-BUILD`, which will
be called with the package name as a string; their return values will
then be used instead.

Note that if it makes no sense to build a package, then you should put
`:build nil` in its [recipe][#user/recipes], rather than specifying
`NO-BUILD` every time you register it with `straight.el`. (This is
especially relevant when writing recipes for [recipe
repositories][#user/lookup/repos].)

#### Variants of `straight-use-package`

For convenience, `straight.el` provides some functions that wrap
`straight-use-package` with particular arguments, to cover all of the
common cases. Each of these functions takes only a package name or
recipe, and no additional arguments.

* `straight-register-package`: always stop after the registration
  step. This may be useful for specifying the recipe for an optional
  dependency (see [recipe lookup][#user/lookup], but see also [recipe
  overrides][#user/overriding]).
* `straight-use-package-no-build`: always stop before the build step.
  This is used by [`straight-freeze-versions`][#user/lockfiles] to
  make sure packages are cloned, since building them is unnecessary
  for writing the lockfiles.
* `straight-use-package-lazy`: stop at the clone step if the package's
  local repository is not already cloned. This is used for
  lazy-loading.

#### Customizing when packages are built

By default, when `straight.el` is bootstrapped during Emacs init, it
uses a bulk `find(1)` command to identify files that were changed
since the last time a package depending on them was built. These
packages are then rebuilt when they are requested via
`straight-use-package`. Normally, `straight.el` will try to detect
what sort of `find(1)` program is installed, and issue the appropriate
command. If it makes a mistake, then you can manually customize
`straight-find-flavor`. Alternately, you can install GNU find and
customize the variable `straight-find-executable` to point to it.

For about 100 packages on an SSD, calling `find(1)` to detect
modifications takes about 500ms. You can save this time by customizing
`straight-check-for-modifications`. This is a list of symbols which
determines how `straight.el` detects package modifications. The
default value is `(find-at-startup find-when-checking)`, which means
that `find(1)` is used to detect modifications at startup, and also
when you invoke `M-x straight-check-package` or `M-x
straight-check-all`. If you prefer to avoid this performance hit, or
do not have `find(1)` installed, then you can remove these symbols
from the list. In that case, you will probably want to add either
`check-on-save` or `watch-files` to the list.

`check-on-save` causes `straight.el` to use `before-save-hook` to
detect package modifications as you make them (modifications made by
the `straight.el` repository management commands are also detected).
This reduces init time, but modifications made outside of Emacs (or
modifications that bypass `before-save-hook`) are not detected. Pull
requests extending the number of cases in which `straight.el` is able
to detect live modifications are welcome. Also, for the sake of
efficiency, this form of modification checking is restricted to
subdirectories of `~/.emacs.d/straight/repos`, so you must put your
local repositories into that directory for it to work. (Pull requests
to change this would be welcome.)

`watch-files` causes `straight.el` to automatically invoke a
filesystem watcher to detect modifications as they are made, inside or
outside of Emacs. For this setting to work, you must have
[`python3`][python] and [`watchexec`][watchexec] installed on your
`PATH`. By default, the watcher persists after Emacs is closed. You
can stop it manually by running `M-x straight-watcher-stop`, and start
it again by running `M-x straight-watcher-start`. The watcher script
is designed so that when one instance is started, all the others
gracefully shut down, so you don't have to worry about accidentally
ending up with more than one. There is nothing exciting in the process
buffer for the watcher, but if you are interested in it then its name
is given by `straight-watcher-process-buffer`. (By default, the name
has a leading space so that the buffer does not appear in the buffer
list.)

There is probably no good reason to use both `check-on-save` and
`watch-files` at the same time. Your configuration can dynamically
switch between which one is used depending on `(executable-find
"watchexec")` or similar.

If you prefer to eschew automatic package rebuilding entirely, you can
just set `straight-check-for-modifications` to `nil`. In that case,
packages will only be rebuilt when metadata (e.g. the recipe or the
Emacs version) changes, or when you manually invoke `M-x
straight-rebuild-package` or `M-x straight-rebuild-all`.

Regardless of your preferred setting for
`straight-check-for-modifications`, you should set it before the
`straight.el` bootstrap snippet is run, since hooks relating to this
variable are set during bootstrap.

On Microsoft Windows, `find(1)` is generally not available, so the
default value of `straight-check-for-modifications` is instead
`(check-on-save)`.

##### Custom or manual modification detection

You can also use the low-level functions for modification detection
directly.

The function `straight-register-repo-modification` takes a string
(e.g. `"straight.el"`) corresponding to the name of a local
repository, and marks all packages from that local repository to be
rebuilt at next Emacs startup. This function silently ignores local
repositories which contain slashes, a limitation which might be
removed in future.

The function `straight-register-file-modification` takes no arguments
and checks if the file visited by the current buffer (if any) is
contained by any local repository. If so, it delegates to
`straight-register-repo-modification`. The `check-on-save` value for
`straight-check-for-modifications` just adds
`straight-register-file-modification` to `before-save-hook`.

##### Summary of options for package modification detection
###### `find-at-startup`

Save build timestamps and run `find(1)` at startup to detect changes

* Most reliable, never misses changes
* Requires `find(1)`
* Slows down startup

###### `check-on-save`

Use `before-save-hook` to detect changes

* No external dependencies
* No startup delay
* No additional CPU or memory impact
* Doesn't catch changes made except via `save-file` inside Emacs

###### `watch-files`

Run filesystem watcher to detect changes

* Requires Python 3 and Watchexec
* No startup delay
* Takes a few seconds to build virtualenv the first time
* Memory and CPU impact of running filesystem watcher
* Only misses changes if you make them after booting the system but
  before starting Emacs

---

#### Customizing how packages are built

By specifying `:build nil` in a package's [recipe][#user/recipes], you
may prevent the package from being built at all. This is usually
useful for recipe repositories which do not bundle executable Lisp
code. (Make sure to use [`straight-use-recipes`][#user/lookup/repos]
for registering recipe repositories.)

##### Autoload generation

By specifying `:build (:not autoloads)` in a package's recipe, you may
prevent any autoloads provided by the package from being generated and
loaded into Emacs. This is mostly useful if the package provides a
large number of autoloads, you know you need only a few of them, and
you wish to optimize your startup time (although this is almost
certainly premature optimization unless you *really* know what you're
doing). You can also customize the variable
`straight-disable-autoloads` to effect this change on all recipes
which do not explicitly disable autoloads via the `:build` keyword.

##### Byte compilation

By specifying `:build (:not compile)` in a package's recipe, you may
inhibit byte-compilation. See [this issue][#357] for discussion of why
this might be useful. You can also customize the variable
`straight-disable-compile` to effect this change on all
recipes which do not explicitly disable byte-compilation via the
`:build` keyword.

##### Native compilation

Experimental support for native compilation of Emacs Lisp code can be
enabled in the latest `master` branch of the official Emacs repository
(see [gccemacs][gccemacs]). When running on this version of Emacs,
`straight.el` will perform native compilation of packages.

By specifying a `:build (:not native-compile)` in a package's recipe,
you may inhibit native compilation. You can also customize the
variable `straight-disable-native-compile` to effect this change on
all recipes which do not explicitly disable native-compilation via the
`:build` keyword.

Native compilation requires byte-compilation, so `:build (:not compile)`
and `straight-disable-compile` will also disable native
compilation.

##### Symbolic links

Usually, `straight.el` uses symbolic links ("symlinks") to make
package files available from the build directory. This happens when
`straight-use-symlinks` is non-nil, the default. On Microsoft Windows,
however, support for symlinks is not always available, so the default
value of `straight-use-symlinks` is nil on that platform. That causes
copying to be used instead, and an advice is placed on `find-file` to
cause the copied files to act as symlinks if you try to edit them.

If you want to activate symlink-support on MS Windows 7, 8, or 10, you
should ensure the following requirements:

* `straight-use-symlinks` has to be set to non-nil manually.

* Your user-account needs to be assigned the right to create symbolic
  links. To do so, run "secpol.msc" and in "Local Policies → User
  Rights Assignment" assign the right to "Create symbolic links" to
  your user-account.

* If you have User Account Control (UAC) enabled and your user-account
  belongs to the the _Administrators_ group you'll need to run Emacs
  in elevated mode to be able to create symlinks (see
  [here][symlinks-perforce] and [here][symlinks-stackoverflow] and,
  for an official reference, section Access Token Changes [in this
  document][symlinks-microsoft].

* [Windows Creators Update][symlinks-creators] supports
  symlink-creation without any special permission setup.


#### Customizing how packages are made available

By setting the variable `straight-cache-autoloads` to a non-nil value,
you can cause `straight.el` to cache the autoloads of all used
packages in a single file on disk, and load them from there instead of
from the individual package files if they are still up to date. This
reduces the number of disk IO operations during startup from O(number
of packages) to O(1), so it should improve performance. No other
configuration should be necessary to make this work; however, you may
wish to call [`straight-prune-build`][#user/interactive] occasionally,
since otherwise this cache file may grow quite large over time.

#### Hooks run by `straight-use-package`

Currently, `straight-use-package` supports four hooks:

* `straight-vc-git-post-clone-hook`: The functions in this hook are
  run just after cloning a git repository.  This allows users to
  automate custom configuration of Elisp Git repositories after they
  have been cloned.  For example, the `user.email` `git-config`
  variable could be set on clone, to make upstream contributions more
  convenient for developers who use different email addresses for
  different repositories.

  Each hook function is passed the following [keyword arguments]:

    - `:repo-dir` - the local directory to which the repository was
      cloned
    - `:remote` - the name of the remote from which the repository was
      cloned
    - `:url` - the URL from which the repository was cloned
    - `:branch` - the branch as specified by the recipe, if any,
      otherwise `nil`
    - `:depth` - the clone depth as specified by the recipe or
      `straight-vc-git-default-clone-depth`
    - `:commit` - the specific commit which was requested via the
      lockfile, if any, otherwise `nil`

  Since keyword arguments are used, each function should be defined
  via `cl-defun`, and `&key` used at the front of the argument list.

* `straight-use-package-prepare-functions`: The functions in this hook
  are run just before a package would be built, even if the package
  does not actually need to be rebuilt. They are passed the name of
  the package being (maybe) built as a string, and should take and
  ignore any additional arguments.
* `straight-use-package-pre-build-functions`: The functions in this
  hook are run just before building a package (and only if the package
  needs to be built). They are passed the name of the package being
  built as a string, and should take and ignore any additional
  arguments.
* `straight-use-package-post-build-functions`: The functions in this
  hook are run just after building a package (and only if the package
  needs to be built). They are passed the name of the package being
  built as a string, and should take and ignore any additional
  arguments.

### The recipe format

The general format for a `straight.el` recipe is:

```emacs-lisp
(package-name :keyword value :keyword value ...)
```

Note that if you wish to pass a recipe to `straight-use-package`, you
will need to quote it. If you need to compute part of the recipe
dynamically, use backquoting:

```emacs-lisp
(straight-use-package
  `(el-patch :type git :repo ,(alist-get 'el-patch my-package-urls)))
```

The supported keywords are *similar, but not identical* to those used
in MELPA recipes. There is a complete list below which you can compare
with the [MELPA documentation][melpa-recipe-format], but the main
differences from the user's point of view are:

* We use `:host` instead of `:fetcher`.
* We only support Git recipes by default, although the system is
  extensible to other VCs to be added in the future or in user
  configurations. Thus the supported `:host` values are:
  * `nil` (any Git repository)
  * `github`, `gitlab`, `sourcehut`, `codeberg`, or `bitbucket`.
* We support `:branch`, but not `:commit` or `:version-regexp`. To
  lock a package to a specific commit, use a
  [lockfile][#user/lockfiles]. See also [#246] for discussion of
  extensions to the recipe to support package pinning, which is a
  planned feature.
* We support several additional keywords that affect how a package is
  built; see below.
* There are consistency and feature improvements to edge cases of the
  `:files` keyword as documented in `straight-expand-files-directive`.

* `:includes` indicates a package is a superset of another package.

Here is a comprehensive list of all keywords which have special
meaning in a recipe (unknown keywords are ignored but preserved):

* `:local-repo`

  This is the name of the local repository that is used for the
  package. If a local repository by that name does not exist when you
  invoke `straight-use-package`, one will be cloned according to the
  package's [version-control settings][#user/recipes/vc-backends].

  Multiple packages can use the same local repository. If so, then a
  change to the local repository will cause both packages to be
  rebuilt. Typically, if multiple packages are drawn from the same
  repository, both should specify a `:files` directive.

  If you do not provide `:local-repo`, then it defaults to a value
  derived from the [version-control
  settings][#user/recipes/vc-backends], or as a last resort the
  package name.

* `:files`

  This is a list specifying which files in a package's local
  repository need to be symlinked into its build directory, and how to
  arrange the symlinks. For most packages, the default value
  (`straight-default-files-directive`) will suffice, and you do not
  need to specify anything.

  If you do need to override the `:files` directive (this happens most
  commonly when you are taking a single package from a repository that
  holds multiple packages), it is almost always sufficient to just
  specify a list of globs or filenames. All matching files will be
  linked into the top level of the package's build directory.

  In spite of this, the `:files` directive supports an almost
  comically powerful DSL (with nested excludes and everything!) that
  allows you full flexibility on how the links are made; see the
  docstring of `straight-expand-files-directive` for the full details.

* `:flavor`

  If this is non-nil, then it changes the interpretation of the
  `:files` directive for compatibility with an existing upstream
  recipe source. See the docstring of
  `straight-expand-files-directive` for details.

* `:build`
  This specifies the steps taken on the files symlinked within a
  package's straight/build/PACKAGE directory. It may be any of the
  following values:

  - nil, in which case nothing is done.
    This also prevents :pre/:post-build commands from running.

```emacs-lisp
(example :build nil)
```

  - t, runs the steps listed in `straight--build-default-steps`.
    Note this ignores all `straight-disable-SYMBOL` keywords.

```emacs-lisp
(example :build t)
```
  - A list of steps.
    Each step is a symbol which represents a function named:
    `straight--build-SYMBOL`.
    The function is passed the recipe as its sole argument.
    Steps are exectuted in the order they are listed. e.g.

```emacs-lisp
(example :build (autoloads compile native-compile info))
```

  - A list which has `:not` as its car and step symbols as its cdr.
    This eliminates the listed steps from the default steps. e.g.
    The following recipe will not be compiled or have its texinfo generated:

```emacs-lisp
(example :build (:not compile info))
```

   Steps may be disabled globally for recipes which do not explicilty
   declare their `:build` via the defcustom variables named
   `straight--build-SYMBOL`. e.g. The last example but for all recipes
   without a `:build`:

```emacs-lisp
(setq straight-disable-compile t
      straight-disable-info t)
```

In the absence of a `:build` keyword, `straight--build-default-steps` are run.

* `:pre-build`

  This specifies system commands and/or elisp to be evaluated before
  symlinking, and running a recipe's `:build` steps.

  Each command is either an elisp form to be evaluated or a list of
  strings to be executed in a shell context of the form:

```emacs-lisp
("executable" "arg"...)
```

  Commands are executed in the package's repository directory.

  The `:pre-build` keyword's value may be:

  - A single command
  - A list of commands
  - nil, in which case no commands are executed.

    For example:

```emacs-lisp
(straight-use-package
 '( example :type git :host github :repo "user/example.el"
    :pre-build ("make" "all")))

(straight-use-package
 `( example :type git :host github :repo "user/example.el"
    :pre-build ,(pcase system-type
                  (`windows-nt '(message "This might take a while"))
                  (_ '(("./configure") ("make") ("make" "install"))))))

```

* `:post-build`

  This specifies system commands and/or elisp to be evaluated after
  the `:build` steps are run.

  Otherwise, it is identical to the `:pre-build` keyword in terms of the values
  it accepts and how it is executed.

    For example:

```emacs-lisp
(straight-use-package
 '( example :type git :host github :repo "user/example.el"
    :pre-build  (("./pre-build.sh") (message "hi"))
    :post-build (("./post-build.sh") (message "bye"))))
```

* `:type`

  This specifies the version-control backend to use for cloning and
  managing the package's local repository. It defaults to the value of
  `straight-default-vc`, which defaults to `git`.

  The only traditional version-control backend currently supported is
  `git`, although more backends may be added.

  As a special case, however, you may specify the value `built-in`,
  which results in all version-control operations on the package being
  ignored. This allows you to tell `straight.el` that a package has
  already been provided (for example, because a version of it is
  shipped by Emacs) and does not have a local repository which needs
  to be cloned, updated, and so on. Here is how you can tell
  `straight.el` that you would like to use the Emacs-provided copy of
  Org, rather than cloning it from the upstream repository if another
  package declares it as a dependency:

```emacs-lisp
(straight-use-package '(org :type built-in))
```

  You can also use `:type nil`, which has the same effect as `:type
  'built-in`, except that the package is still loaded from its
  configured `:local-repo`.

* `:source`

 Overrides `straight-recipe-repositories` on a per-recipe basis.
 Its value may be:
   - a symbol representing a recipe repository
   - a list of such symbols
 The order of the symbols determines their precedence. For example:

```emacs-lisp
(straight-use-package '(package :source melpa))
```

 Will search only the melpa recipe repository for package's recipe. While:

```emacs-lisp
(straight-use-package '(package :source (melpa gnu-elpa-mirror)))
```

 will search for package's recipe first in melpa.
 If it is not found there it will check gnu-elpa-mirror next.

* backend-specific keywords

  Depending on the value of `:type`, additional keywords (relevant to
  how the package's repository is cloned and managed) will be
  meaningful. See the next section.

  The `built-in` and `nil` pseudo-backends do not take any other
  keywords.

* `:includes`

Informs `straight.el` that a package is a superset of another package.
For example `org-contrib` includes `ol-vm`.
The following will prevent `straight.el` from attempting to install `ol-vm`
after `org-contrib` has been installed:

```emacs-lisp
(straight-use-package '(org-contrib :includes ol-vm))
```

Its value may also be a list of symbols indicating multiple packages:

```emacs-lisp
(straight-use-package '(example :includes (foo bar)))
```

* `:inherit`

Overrides `straight-allow-recipe-inheritance` on a per-recipe basis.
If its value is non-nil, inheritance is enabled for the recipe.
Otherwise it is not.

#### Version-control backends

Defining a version-control backend consists of declaring a number of
functions named as `straight-vc-BACKEND-METHOD`, where `BACKEND` is
the name of the version-control backend being defined and `METHOD` is
a backend API method. The relevant methods are:

* `clone`: given a recipe and a commit object, clone the repository
  and attempt to check out the given commit.
* `commit-present-p`: given a recipe and a commit object, return
  whether the commit can be checked out offline, i.e., without
  fetching from the remote.
* `normalize`: given a recipe, "normalize" the repository (this
  generally means reverting it to a standard state, such as a clean
  working directory, but does not entail checking out any particular
  commit).
* `fetch-from-remote`: given a recipe, fetch the latest version from
  its configured remote, if one is specified.
* `fetch-from-upstream`: given a recipe, fetch the latest version from
  its configured upstream, if the package is forked.
* `merge-from-remote`: given a recipe, merge the latest version
  fetched from the configured remote, if any, to the local copy.
* `merge-from-upstream`: given a recipe, merge the latest version
  fetched from the configured upstream, if the package is forked, to
  the local copy.
* `push-to-remote`: given a recipe, push the current version of the
  repository to its configured remote, if one is specified.
* `check-out-commit`: given a recipe and a commit object, attempt to
  check out that commit in the repository for that recipe.
* `get-commit`: given a local repository name, return the commit
  object that is currently checked out, or nil if the local repository
  should not be included in a lockfile.
* `local-repo-name`: given a recipe, return a good name for the local
  repository, or nil.
* `keywords`: return a list of keywords which are meaningful for this
  version-control backend.

Most of these methods are highly interactive: they don't actually do
anything without prompting you to confirm it, and very often they will
offer you a number of different options to proceed (including starting
a recursive edit and allowing you to do whatever you would like).

Also, all of the methods in this section take [`straight.el`-style
recipes][#dev/recipe-formats]; see the section on [defining VC
backends][#dev/vc-backends] in the developer manual for more details.

#### Git backend

These are the keywords meaningful for the `git` backend:

* `:repo`: the clone URL for the Git repository.
* `:host`: either nil or one of the symbols `github`, `gitlab`,
  `bitbucket`, `codeberg`, or `sourcehut`.
    * If nil, then `:repo` should be a string which is the full URL of
    the target repository. For example:

    ```emacs-lisp
    ( :package "package" :host nil :type git
      :repo "http://myhost.tld/repo")
    ```

    * If non-nil, then `:repo` should be a string "username/repo",
    and the URL is constructed automatically.  For example:

    ```emacs-lisp
    ( :package "package" :host github :type git
      :repo "username/repo")
    ```

* `:branch`: the name of the branch used for primary development, as a
  string. If your version lockfiles do not specify a commit to check
  out when the repository is cloned, then this branch is checked out,
  if possible. This branch is also viewed as the "primary" branch for
  the purpose of normalization and interaction with the remote.
* `:remote`: the name to use for the Git remote. If the package is
  forked, this name is used for the upstream remote.
* `:nonrecursive`: if non-nil, then submodules are not cloned. This is
  particularly important for the Emacsmirror recipe repository, which
  contains every known Emacs package in existence as submodules.
* `:fork`: the settings for a fork, if desired.
  This causes the `fetch-from-remote` method to operate on the fork;
  you can use the `fetch-from-upstream` method to operate on the
  upstream instead.

  Note: the following section assumes `straight-host-usernames`
  has a value of:

```emacs-lisp
'((github    . "githubUser")
  (gitlab    . "gitlabUser")
  (codeberg  . "codebergUser")
  (sourcehut . "sourcehutUser")
  (bitbucket . "bitbucketUser")))
```

  Its value may be:

  * `t`:
  Look up the username in `straight-host-usernames`.
  Inherit the repository name from the upstream repository.
  For example:

```emacs-lisp
( :package "package" :host github :type git :repo "upstream/repo"
  :fork t)
```

  computes the fork's `:repo` value as `githubUser/repo`.

  * a string (optionally ending with "/"):
  Use the string as the username.
  Inherit repository name from the upstream repository.
  For example:

```emacs-lisp
( :package "package" :host github :type git :repo "upstream/repo"
  :fork "user")
```

  computes the fork's `:repo` value as `user/repo`.

  * a string starting with "/":
  Look up the username in `straight-host-usernames`.
  Use the string as the repository name.
  For example:

```emacs-lisp
( :package "package" :host github :type git :repo "upstream/repo"
  :fork "/renamed")
```

  computes the fork's `:repo` value as `githubUser/renamed`.

  * a string with both the recipe and repository specified:
  Use string as the `:repo` value for the fork.
  For example:

```emacs-lisp
( :package "package" :host github :type git :repo "upstream/repo"
  :fork "user/renamed")
```

  computes the fork's `:repo` value as `user/renamed`.

  * a plist:
  The allowed keywords are `:repo`, `:host`, `:branch`, and `:remote`.
  The same rules as above apply for the `:repo` string.
  Likewise, if the `:host` is overridden and the `:repo` does not
  provide the username, it is looked up in `straight-host-usernames`.
  For example:

```emacs-lisp
( :package "package" :host github :type git :repo "upstream/repo"
  :fork (:host gitlab))
```

  computes the fork's `:repo` value as `gitlabUser/repo`.

```emacs-lisp
( :package "package" :host github :type git :repo "upstream/repo"
  :fork (:host gitlab :repo "/renamed"))
```

  computes the fork's `:repo` value as `gitlabUser/renamed`.

```emacs-lisp
( :package "package" :host github :type git :repo "upstream/repo"
  :fork (:host gitlab :repo "user"))
```

  computes the fork's `:repo` value as `user/repo`.

* `:depth`: either the symbol `full` or an integer. If `full`, then
  the repository is cloned with its whole history. If an integer `N`,
  then the repository is cloned with the option `--depth N`. This
  works even when a commit is specified (e.g. by version lockfiles).
  The default value is `full`.
* `:protocol`: If non-nil, force this protocol to be used when
  interacting with the remote repository. Takes the same values as
  `straight-vc-git-default-protocol`.

This section tells you how the `git` backend, specifically, implements
the version-control backend API:

* `clone`: clones the repository, including submodules if
  `:nonrecursive` is not provided. Checks out the commit specified in
  your revision lockfile, or the `:branch` (from the `:fork`
  configuration, if given), or `origin/HEAD`. If a `:fork` is
  specified, also fetches from the upstream.
* `commit-present-p`: checks if the commit SHA is among the revisions
  that are present locally.
* `normalize`: verifies that remote URLs are set correctly, that no
  merge is in progress, that the worktree is clean, and that the
  primary `:branch` (from the `:fork` configuration, if given) is
  checked out.
* `fetch-from-remote`: checks that remote URLs are set correctly, then
  fetches from the primary remote (the fork, if the package is
  forked).
* `fetch-from-upstream`: checks that remote URLs are set correctly,
  then fetches from the upstream remote. If the package is not a fork,
  does nothing.
* `merge-from-remote`: performs normalization, then merges from the
  primary remote (the fork, if the package is forked) into the primary
  local `:branch`.
* `merge-from-upstream`: performs normalization, then merges from the
  upstream remote into the primary local `:branch`. If the package is
  not a fork, does not attempt to merge.
* `push-to-remote`: performs normalization, pulls from the primary
  remote if necessary, and then pushes if necessary. This operation
  acts on the fork, if the package is forked.
* `check-out-commit`: verifies that no merge is in progress and that
  the worktree is clean, then resets the worktree to the specified
  commit.
* `get-commit`: returns HEAD as a 40-character string.
* `local-repo-name`: if `:host` is non-nil, then `:repo` will be of
  the form "username/repository", and "repository" is used. Otherwise,
  if the URL is of the form `.../<something>.git`, then `<something>`
  is used. Otherwise, nil is returned.
* `keywords`: see the list of keywords above.

You can customize the following user options:

* `straight-vc-git-default-remote-name`: the name to use for the
  primary remote, or the upstream remote if the package is forked.
  Defaults to "origin". The `:remote` keyword may be used to override
  the value of this variable on a per-repository basis.
* `straight-vc-git-default-fork-name`: the name to use for the fork
  remote, if the package is forked. Defaults to "fork". The `:remote`
  keyword may be used to override the value of this variable on a
  per-repository basis.
* `straight-vc-git-default-protocol`: the default protocol to use for
  automatically generated URLs when `:host` is non-nil. It can be
  either `https` or `ssh`, and defaults to `https` because this
  requires less work to set up.
* `straight-vc-git-force-protocol`: if this is non-nil, then HTTPS and
  SSH URLs are not treated as equivalent, so that bulk version-control
  operations will offer to re-set your remote URLs from HTTPS to SSH
  or vice versa, depending on the value of
  `straight-vc-git-default-protocol`. This is nil by default.
* `straight-vc-git-auto-fast-forward`: if this is non-nil, pulling
  will quietly do fast-forward, to suppress asking for instructions on
  each package with updates, unless they're not trivial. Set to nil if
  you'd prefer to inspect all changes.
* `straight-vc-git-default-clone-depth`: the default value for the
  `:depth` keyword. It can be either the symbol `full` or an integer,
  and defaults to `full`. Setting this variable to a small integer will
  reduce the size of repositories. This variable affects all packages,
  even those whose versions are locked.

  Please be careful with setting `straight-vc-git-default-clone-depth`,
  which may break some packages' installing processes such as `elfeed`
  that depend on `org`.

##### Deprecated `:upstream` keyword

`straight.el` previously supported fork configuration in recipes using
an `:upstream` keyword rather than a `:fork` keyword. For various
reasons, this was more complex to handle, which is why the change was
made. For backwards compatibility, the `:upstream` keyword is still
accepted, with the following behavior.

When `straight.el` processes a recipe which uses the `:upstream`
keyword, it moves the `:repo`, `:host`, and `:branch` keywords from
that sub-plist to the top level, and moves those top-level keywords to
a new `:fork` sub-plist. Then it sets the top-level and `:fork`
sub-plist values of `:remote` to the values of the deprecated
variables `straight-vc-git-upstream-remote` (defaults to "upstream")
and `straight-vc-git-primary-remote` (defaults to "origin"),
respectively.

For backwards compatibility, if `straight-vc-git-primary-remote`
differs from its default value of "origin", then its value is used in
place of `straight-vc-git-default-remote-name`.

### Recipe lookup

If you only provide a symbol (package name) to `straight-use-package`,
then the recipe is looked up automatically. By default, [MELPA], [GNU
ELPA][gnu-elpa], and [Emacsmirror] are searched for recipes, in that
order. This means that one or more of them may need to be cloned.
Recipe repositories are actually just the same as ordinary packages,
except that their recipes specify `:build nil`, so they are not
symlinked or added to the `load-path`.

Note that dependencies always use the default recipes, since the only
information `straight.el` gets about a package's dependencies are
their names.

This leads to a few interesting questions regarding requesting a
package multiple times. For example, you might need to load two
features using [`use-package`][use-package] that are provided from the
same package, or one of the packages you have installed is also
requested as a dependency by another package. `straight.el` uses a
number of heuristics to try to make these interactions as intuitive
and painless as possible:

* The first time a package is registered with `straight.el`, its
  recipe (either the recipe that you provided, or the one that was
  looked up from a recipe repository) is recorded. In future
  registration, if you just provide the package name to
  `straight-use-package`, the existing recipe is reused.

  Note, however: *if* you want to use a custom recipe for a given
  package, you must load it *before* all of its dependencies.
  Otherwise, the package will first be registered as a dependency,
  using the default recipe.

* If a package has already been registered with `straight.el`, and you
  attempt to load it again with an explicit recipe which is different
  from the one previously registered, the new recipe is used but a
  warning is signalled.

* If you attempt to register a package which shares a `:local-repo`
  (either by default, or due to explicit specification) with a
  previously registered package, and the two packages specify
  different values for their version-control keywords (see
  [version-control backends][#user/recipes/vc-backends]), then the new
  recipe is used but a warning is signalled. If the repository was
  already cloned, this means the second recipe will have no effect.

  But if the second recipe was fetched automatically from a recipe
  repository, all of its version-control keywords will be silently
  overwritten with the ones from the first recipe, to avoid conflicts
  (although if there are conflicts in other parts of the recipe, a
  warning will still be displayed).

#### Updating recipe repositories

As mentioned in the [conceptual overview][#concepts/lookup], recipe
repositories are just regular packages, with some extra code to look
up recipes in the relevant local repository.

This means that updating a recipe repository may be done the same way as
updating a regular package, i.e. with [`M-x
straight-pull-package`][#user/interactive/vc].
A convenience command with interactive completion for recipe repositories,
`straight-pull-recipe-repositories`, is provided as well.
You should use one of these if you find that a package isn't listed by `M-x
straight-use-package`—perhaps it was added recently.

Note that there is currently some potentially surprising behavior if
you update all packages at once using `M-x straight-pull-all` or `M-x
straight-merge-all`, and this bulk update includes recipe repository
updates: see [#323].

#### Customizing recipe repositories

The recipe repository system is designed to be extended. Firstly, you
can control which recipe repositories are searched, and in what order
of precedence, by customizing `straight-recipe-repositories`. The
default value is:

```emacs-lisp
(org-elpa melpa gnu-elpa-mirror el-get emacsmirror)
```

##### GNU ELPA

You can customize the following user options:

* `straight-recipes-gnu-elpa-url`: The Git URL to use for the
  `gnu-elpa` recipe repository.
* `straight-recipes-gnu-elpa-use-mirror`: GNU ELPA uses a stupidly
  complex build system for no good reason, and it's unlikely to change
  any time soon. What this means for you is that you have to run the
  Elisp-powered Makefile of the GNU ELPA repository (which has a fatal
  bug last I checked, so you'd have to patch it locally) *and* have a
  full checkout of the Emacs source repository (more than 1GB) if you
  want all the packages to work correctly. To work around this
  problem, I maintain a [full mirror of GNU ELPA on
  GitHub][gnu-elpa-mirror]. (The tooling used to maintain this mirror
  is located [here][gnu-elpa-mirror-tool].) By default, `straight.el`
  retrieves packages from this mirror instead of the source
  repository; this behavior is controlled by the value of
  `straight-recipes-gnu-elpa-use-mirror`. You must do any
  customization of this variable *before* the `straight.el`
  [bootstrap][#quickstart]. Note that setting the value of this user
  option to nil causes the default value of
  `straight-recipe-repositories` to shift to:

```emacs-lisp
(org-elpa melpa gnu-elpa el-get emacsmirror)
```

##### Emacsmirror

You can customize the following user option:

* `straight-recipes-emacsmirror-use-mirror`: Yes, there is also a
  mirror for Emacsmirror. This is because the [epkgs] repository
  contains a (frequently updated) SQLite database in it, which means
  the Git repository takes *forever* to clone (see [#356]). My
  solution to this problem is to generate a new repository which
  contains the information that `straight.el` needs but which is much
  smaller. By default, `straight.el` uses the official [epkgs]
  repository to find packages on Emacsmirror, but you can tell it to
  use my mirror by configuring the value of this variable to non-nil.
  You must do any customization of this variable *before* the
  `straight.el` [bootstrap][#quickstart]. Note that setting the value
  of this user option to non-nil causes the default value of
  `straight-recipe-repositories` to shift to:

```emacs-lisp
(org-elpa melpa gnu-elpa-mirror el-get emacsmirror-mirror)
```

##### Defining new recipe repositories

To define a new recipe repository called `NAME`, you should do the
following things:

* Define a function `straight-recipes-NAME-retrieve`, which takes a
  package name as a symbol and returns a recipe for that package if it
  is available, else nil. This is used for recipe lookup. This
  function may assume that the local repository for the recipe
  repository has already been cloned, and that `default-directory` has
  been set to that local repository. This is used for recipe lookup
  during the course of `straight-use-package`.

  If the returned recipe is a backquoted list, it will be evaluated
  during `straight--convert-recipe`. This is useful for specifying
  dynamic elements within the recipe such as system-specific
  build commands. For example, if `straight-recipes-NAME-retrieve`
  returns:

```emacs-lisp
'`( package :type git :repo "host/repo"
    :pre-build ,(pcase system-type
                  (`berkeley-unix '("gmake"))
                  (_ '("make")))
    :files (:defaults))
```

  The recipe is converted to:

```emacs-lisp
(package :type git :repo "host/repo"
 :pre-build ("make")
 :files (:defaults))
```

  on a `gnu/linux` system, and:

```emacs-lisp
(package :type git :repo "host/repo"
         :pre-build ("gmake")
         :files (:defaults))
```

  on a `berkely-unix` system.

  The recipe could be read from a file in the recipe repository as
  well. In this case, the quote is *not* included in the recipe, as
  `straight-recipes-NAME-retrieve` would make use of `read`, which
  will return the literal Lisp object. For example, considering the
  following retrieval function:

```emacs-lisp
(defun straight-recipes-example-retrieve (name)
  (with-temp-buffer
    (insert-file-literally "./recipes/example.recipe")
    (read (buffer-string))))
```

  The recipe from above could be stored in the file, `example.recipe`, as:

```emacs-lisp
`( package :type git :repo "host/repo"
   :pre-build ,(pcase system-type
                 (`berkeley-unix '("gmake"))
                 (_ '("make")))
   :files (:defaults))
```

* Define a function `straight-recipes-NAME-list`, which takes no
  arguments and returns a list of strings representing packages for
  which recipes are available. It is permissible to return some
  strings for which recipes are actually not available, for
  performance reasons. However, this is discouraged. (The [MELPA]
  backend uses this functionality, since all files in the `recipes`
  directory are potentially recipes, but only the Git-based ones can
  actually be used.)
* (Optional) Define a function `straight-recipes-NAME-version` which
  returns a non-nil value indicating the current version of the logic
  in your `straight-recipes-NAME-retrieve` function. Each time you
  change the logic, this version value must be changed. If this
  function is defined, then `straight.el` automatically and
  transparently caches calls to `straight-recipes-NAME-retrieve`
  persistently, using your version value (and its detection of
  modifications to the recipe repository) to decide when to invalidate
  the cache.
* Call `straight-use-recipes` with the recipe for your recipe
  repository. Make sure to include `:build nil` in the recipe, unless
  you also want to use the recipe repository as an executable Emacs
  Lisp package. Alternatively, you can take the manual approach:
    * Call `straight-use-package-lazy` with the recipe for your recipe
      repository.
    * Add the symbol for your recipe repository's name (the car of the
      recipe you provided, that is) to `straight-recipe-repositories`,
      at the appropriate place.

### Overriding recipes

You can always use `straight-register-package` to specify a specific
recipe for a package without cloning or building it, so that just in
case that package is requested later (possibly as a dependency, or in
somebody else's code) your recipe will be used instead of the default
one. However, this does not help in the case that a specific recipe is
passed to `straight-use-package`.

Also, it is obviously impossible to call `straight-register-package`
before `straight.el` has been loaded, so you can't use it to specify a
custom recipe for `straight.el` itself.

To remedy these difficulties, `straight.el` provides a mechanism for
specifically overriding the recipe for a particular package. You can
use it by customizing `straight-recipe-overrides`, or by calling
`straight-override-recipe`.

`straight-recipe-overrides` is an association list from [profile
names][#user/lockfiles/profiles] to *override alists*. If you don't
care about the profile system, you can just use a single override
specification, with the profile name nil. Each override alist is just
a list of recipes. Because the car of a recipe is just the package
name as a symbol, this list of recipes is also an alist whose keys are
recipe names and whose values are the plists for those recipes.

Even if an explicit recipe is supplied to `straight-use-package`, the
one given in `straight-recipe-overrides` will be used instead, if such
a recipe is specified there.

For convenience, you may add to `straight-recipe-overrides` by passing
a recipe to `straight-override-recipe`. This will register it in the
override alist for the current profile. Note that if you do this, you
will probably want to explicitly set `straight-recipe-overrides` to
nil before bootstrapping `straight.el`. This will make it so that if
you remove a call to `straight-override-recipe` from your init-file
and then reload it, the entry will actually be removed from
`straight-recipe-overrides`.

#### Overriding the recipe for `straight.el`

As was briefly mentioned earlier, you can actually override the recipe
of `straight.el` itself using `straight-recipe-overrides`! How does
this work? Well, it's basically black magic. If you want the details,
go read the [developer manual][#dev/recipe-internals]. All you need to
know is that you can set `straight-recipe-overrides`, and it will
magically work. The only caveat is that if you change the
`:local-repo` for `straight.el`, then you will also need to adjust the
value of `bootstrap-file` in the [bootstrap snippet][#quickstart]
accordingly, since otherwise your init-file will not know where to
find `straight.el`. (You must use `straight-recipe-overrides` instead
of `straight-override-recipe`, since the latter function definition
hasn't been loaded yet before `straight.el` is installed and
bootstrapped.)

Here is the default recipe used for `straight.el`, if you don't
override it:

```emacs-lisp
(straight :type git :host github
          :repo ,(format "%s/straight.el" straight-repository-user)
          :files ("straight*.el")
          :branch ,straight-repository-branch)
```

Note that even though the bootstrap snippet references the `develop`
branch of `straight.el`, the default recipe installs from `master`.

If all you want to do is change which branch you are installing
`straight.el` from, simply customize the variable
`straight-repository-branch`, which is provided for this purpose.
(Although using `straight-recipe-overrides` will work just as well, at
least until the recipe happens to be changed upstream and your
init-file isn't updated.)

Similarly, if all you want to do is switch to your own fork of
`straight.el` on GitHub, simply customize the variable
`straight-repository-user` to your GitHub username.

There is one minor caveat to the above discussion. If your fork makes
changes to the way in which recipes are interpreted, then those
changes will not be effective during the interpretation of your own
recipe for `straight.el`. If you wish for them to be, then you will
have to follow the same procedure that is followed in `straight.el`
itself for making changes to recipe interpretation. These details are
outlined in the [developer manual][#dev/recipe-internals]; see also
`install.el` for an explanation of this aspect of the bootstrap
mechanism.

### Interactive usage

The primary usage of `straight.el` is expected to be in your
init-file. For example, this is where you will need to put the
bootstrap code as well as any packages that you always want to be
installed. However, there are three important interactive uses of
`straight.el`: temporary installation of packages, various helpful
utility functions, and [version control
operations][#user/interactive/vc].

To install a package temporarily, run `M-x straight-use-package`. All
registered recipe repositories will be cloned, and you will be
presented with a combined list of all recipes available from them.
Simply select a package and it will be cloned, built, and loaded
automatically. This does not affect future Emacs sessions.

If you provide a prefix argument to `M-x straight-use-package`, then
you are presented with a list of registered recipe repositories. After
you select one, you are shown a list of recipes specifically from that
recipe repository. This is helpful if you do not want to clone all
registered recipe repositories, or you have a particular recipe
repository in mind.

You can also call `M-x straight-get-recipe`, which has the same
interface as `M-x straight-use-package`, except that instead of the
package being cloned, built, and loaded, its recipe is copied to the
kill ring. If you are writing a custom recipe, this may be helpful,
because you may be able to reuse parts of the existing recipe,
particularly the `:files` directive.

Normally, packages are rebuilt automatically if needed, when Emacs
restarts. If you for some reason want them to be rebuilt at another
time, you can call `M-x straight-check-all` to rebuild all packages
that have been modified since their last build. Alternatively, use
`M-x straight-rebuild-all` to unconditionally rebuild all packages.
Note that this will probably take a while. There are also `M-x
straight-check-package` and `M-x straight-rebuild-package`, which
allow you to select a particular package to check or rebuild.

Finally, you may use `M-x straight-prune-build` in order to tell
`straight.el` to forget about any packages which were not registered
since the last time you loaded your init-file. This may improve
performance, although only slightly, and will clean up stale entries
in the `build` directory. You can call this function in your init-file
if you really wish your filesystem to be as clean as possible,
although it's not particularly recommended as the performance
implications are uninvestigated. If you do call it in your init-file,
be sure to only call it on a fully successful init; otherwise, an
error during init will result in some packages' build information
being discarded, and they will need to be rebuilt next time.

If you have enabled [autoloads caching][#user/install/loading], it is
advisable to call `straight-prune-build` occasionally, since otherwise
the build cache may grow quite large over time.

#### Version control operations

`straight.el` provides a number of highly interactive workflows for
managing your package's local repositories, using the configured
[version-control backends][#user/recipes/vc-backends]. They are as
follows:

* `M-x straight-normalize-package`: normalize a package
* `M-x straight-normalize-all`: normalize all packages
* `M-x straight-fetch-package`: fetch from a package's configured
  remote; with prefix argument, then for forks also fetch from the
  upstream
* `M-x straight-fetch-package-and-deps`: fetch from the configured
  remotes of a package and all of its dependencies (including the
  dependencies of its dependencies); with prefix argment, then for
  forks also fetch from the upstream
* `M-x straight-fetch-all`: fetch from all packages' configured
  remotes; with prefix argument, then for forks also fetch from the
  upstreams
* `M-x straight-merge-package`: merge the latest version fetched from
  a package's configured remote into the local copy; with prefix
  argument, then for forks also merge from the upstream
* `M-x straight-merge-package-and-deps`: merge the latest versions
  fetched from the configured remotes of a package and all of its
  dependencies (including the dependencies of its dependencies); with
  prefix argment, then for forks also merge from the upstreams
* `M-x straight-merge-all`: merge the latest versions fetched from
  each package's configured remote into its local copy; with prefix
  argument, then for forks also merge from the upstreams
* `M-x straight-pull-package`: combination of `M-x
  straight-fetch-package` and `M-x straight-merge-package`
* `M-x straight-pull-package-and-deps`: combination of `M-x
  straight-fetch-package-and-deps` and `M-x
  straight-merge-package-and-deps`
* `M-x straight-pull-all`: combination of `M-x straight-fetch-all` and
  `M-x straight-merge-all`
* `M-x straight-push-package`: push a package to its remote, if
  necessary
* `M-x straight-push-all`: push all packages to their remotes, if
  necessary

See the sections on [version-control
backends][#user/recipes/vc-backends] and the [Git
backend][#user/recipes/git] in particular for more information about
the meanings of these operations.

### Lockfile management

`straight.el` determines your package management configuration from
two, and only two, sources: the contents of your init-file, and your
version lockfiles (which are optional). Your init-file specifies the
configuration of `straight.el` (for example, the values of
`straight-recipe-overrides` and `straight-default-vc`), the packages
you want to use, and their recipes. Your version lockfiles specify the
exact revisions of each package, recipe repository, and even
`straight.el` itself. Together, they lock down your Emacs
configuration to a state of no uncertainty: perfect reproducibility.

To write the current revisions of all your packages into version
lockfiles, run `M-x straight-freeze-versions`. This will first check
that `straight.el` has an up-to-date account of what packages are
installed by your init-file, then ensure that all your local changes
are pushed (remember, we are aiming for perfect reproducibility!). If
you wish to bypass these checks, provide a prefix argument.

**Note: reloading your init-file must have the effect of running all
of the same `straight.el`-related functions again. For example, if you
bootstrap `straight.el` in a sub-file that you only `require` instead
of `load`, then the reloading functionality will not work correctly
and you may receive the message `Caches are still outdated; something
is seriously wrong`. See [#437] for discussion.**

Version lockfiles are written into `~/.emacs.d/straight/versions`. By
default, there will be one, called `default.el`. It is recommended
that you keep your version lockfiles under version control with the
rest of your Emacs configuration. If you symlink your init-file into
`~/.emacs.d` from somewhere else, you should also make sure to symlink
your version lockfiles into `~/.emacs.d/straight/versions`. On a new
machine, do this *before* launching Emacs: that way, `straight.el` can
make sure to check out the specified revisions of each package when
cloning them for the first time.

To install the versions of the packages specified in your version
lockfiles, run `M-x straight-thaw-versions`. Thawing will
interactively check for local changes before checking out the relevant
revisions, so don't worry about things getting overwritten.

#### The profile system

`straight.el` has support for writing multiple version lockfiles,
instead of just one. Why? Consider a large Emacs configuration such as
[Radian], [Spacemacs], or [Prelude], which is used by many different
people. There are two parts to the configuration that is actually
loaded: the "default" part, and the local customizations that each
user has added. Generally, these configurations have a mechanism for
making local customizations without forking the entire project.

So Radian will have some set of packages that it requires, and my
local customizations of Radian have some other set of packages that
they require. In order for me to maintain Radian, I need to be able to
separate Radian's packages (which go into a versions lockfile in the
Radian repository) from my own local packages (which go into a
versions lockfile in my own private local dotfiles repository).
`straight.el` provides this ability through the *profile system*.

The idea is that whenever a package is registered, either directly or
as a dependency, it is associated with a given profile. Any given
package can be associated with multiple profiles.

When you call `straight-use-package`, which profile the registered
packages are associated with is determined by the value of
`straight-current-profile`, which defaults to nil. In Radian, for
example, `straight-current-profile` is bound to `radian` while the
Radian libraries are being loaded, and it is bound to `radian-local`
while the user's local customizations are being loaded. This results
in Radian packages being associated with the `radian` profile, and the
user's local packages being associated with the `radian-local`
profile.

When you call `M-x straight-freeze-versions`, one or more version
lockfiles are written, according to the value of `straight-profiles`.
This variable is an association list whose keys are symbols naming
profiles and whose values are filenames for the corresponding version
lockfiles to be written into `~/.emacs.d/straight/versions`. You
should make sure that each potential value of
`straight-current-profile` has a corresponding entry in
`straight-profiles`, since otherwise some packages might not be
written into your lockfiles.

When customizing [`straight-recipe-overrides`][#user/overriding], note
that if multiple profiles are set to override the same recipe, then
the last one listed in `straight-profiles` will take precedence.
Similarly, when using `M-x straight-thaw-versions`, if different
lockfiles specify revisions for the same local repository, the last
one in `straight-profiles` will take precedence.

### Packages and the init-file

Package managers like `package.el` store mutable state outside your
init-file, including the set of packages that are installed.
`straight.el` does not do this, so it has a rather different way of
determining what packages are installed. To `straight.el`, a package
is part of your Emacs configuration if it is passed to
`straight-use-package` when your init-file is loaded.

Note that this means packages installed interactively (using `M-x
straight-use-package`) are not considered part of your Emacs
configuration, since the invocation of `straight-use-package` does not
happen in your init-file.

This raises an interesting question: if you *add* a package to your
init-file, how can you convince `straight.el` that it really is part
of your init-file, and not just part of a temporary
`straight-use-package` form that you evaluated ad-hoc? The answer is
simple: *reload your entire init-file*. That way, `straight.el` will
see whether or not that package is registered during your init-file.

One might ask how `straight.el` determines that you have finished
loading your init-file. The answer is simple: `post-command-hook` is
used to execute code only after the current interactive operation has
finished. The implementation of this concept is part of the
*transaction system* of `straight.el`, and it is also used to amortize
certain performance costs when many calls to `straight-use-package`
are made sequentially. However, since the transaction system (at least
in recent versions of `straight.el`) operates transparently, its
details are relegated to the [developer manual][#dev/transactions].

### Using `straight.el` to reproduce bugs

#### ... in other packages

One of the major reasons I wanted to write `straight.el` was that
existing package managers were not good for reproducing bugs. For
instance, some of them would load all installed packages when the
package manager was initialized! Obviously that is not acceptable for
a "minimal test case".

On the contrary, bootstrapping `straight.el` does not load anything
except for `straight.el` itself (the default recipe repositories are
registered, but not cloned until needed). You should normally be
loading `straight.el` by means of the [bootstrap
snippet][#quickstart], but when you are in `emacs -Q`, here is how you
can initialize `straight.el`:

```
M-x load-file RET ~/.emacs.d/straight/repos/straight.el/bootstrap.el RET
```

You can also do this from the command line, perhaps by creating an
alias for it:

```
$ emacs -Q -l ~/.emacs.d/straight/repos/straight.el/bootstrap.el
```

Let's say you are making a bug report for Projectile. To load just
Projectile and all of its dependencies, run:

```
M-x straight-use-package RET projectile RET
```

Note that this will use the currently checked-out revisions of
Projectile and all of its dependencies, so you should take note of
those in order to make your bug report.

#### ... in `straight.el` itself

`straight.el` provides a macro, `straight-bug-report`, to test
`straight.el` in a clean environment. If possible, please use this
when creating bug reports.

`straight-bug-report` accepts the following keyword value pairs:

- `:pre-bootstrap (Form)...` Forms evaluated before bootstrapping
    `straight.el` e.g.

```emacs-lisp
(setq straight-repository-branch "develop")
```

    Note this example is already in the default bootstrapping code.

- `:post-bootstrap (Form)...` Forms evaluated in the testing
    environment after boostrapping. e.g.

```emacs-lisp
(straight-use-package '(example :type git :host github))
```

- `:interactive Boolean` If nil, the subprocess will immediately exit
    after the test. Output will be printed to
    `straight-bug-report--process-buffer` Otherwise, the subprocess
    will be interactive.

- `:preserve Boolean` If t, the test directory is left in the
    directory stored in the variable `temporary-file-directory'.
    Otherwise, it is immediately removed after the test is run.

- `:executable String` Indicate the Emacs executable to launch.
    Defaults to `"emacs"`.

- `:raw Boolean` If t, the raw process output is sent to
    `straight-bug-report--process-buffer`. Otherwise, it is formatted
    as markdown for submitting as an issue."

 For example:

```emacs-lisp
(straight-bug-report
  :pre-bootstrap
  (message "before bootstrap")
  (message "multiple forms allowed")
  :post-bootstrap
  (message "after bootstrap")
  (message "multiple forms allowed")
  (straight-use-package '(my-broken-package))
  (message "bye"))
```

The above will run your test in a clean environment and produce a
buffer with information you can paste directly into the issue body.

### Using `straight.el` to develop packages

The workflow for developing a package using `straight.el` is quite
straightforward:
* Add the package to your configuration as usual, via a call to
  `straight-use-package`.
* Use `M-x find-function` or a similar command to jump to the code you
  wish to edit.
* Edit the code.
* Either evaluate the edited code using `M-x eval-buffer`, `M-x
  eval-defun`, or a similar command, or just restart Emacs to pick up
  your changes.
* When you are satisfied with your changes, use [Magit] or just Git
  directly in order to commit and possibly push them. I suggest using
  [Forge] to create pull requests directly from Emacs, with Magit
  integration.

### Integration with other packages
#### Integration with `use-package`

By default, `straight.el` installs a new keyword `:straight` for
`use-package` which may be used to install packages via `straight.el`.
The algorithm is extremely simple. This:

```emacs-lisp
(use-package el-patch
  :straight t)
```

macroexpands (essentially) to:

```emacs-lisp
(straight-use-package 'el-patch)
```

And this:

```emacs-lisp
(use-package el-patch
  :straight (:host github :repo "radian-software/el-patch"
             :branch "develop"))
```

becomes:

```emacs-lisp
(straight-use-package
 '(el-patch :host github :repo "radian-software/el-patch"
            :branch "develop"))
```

If the feature you are requiring with `use-package` is different from
the package name, you can provide a full recipe:

```emacs-lisp
(use-package tex-site
  :straight (auctex :host github
                    :repo "emacsmirror/auctex"
                    :files (:defaults (:exclude "*.el.in"))))
```

And you may also provide just the package name:

```emacs-lisp
(use-package tex-site
  :straight auctex)
```

If you don't provide `:straight`, then by default nothing happens. You
may customize `straight-use-package-by-default` to make it so that
`:straight t` is assumed unless you explicitly override it with
`:straight nil`.

Previously, `straight.el` used a different syntax for its
`use-package` integration. For backwards compatibility, you can use
this syntax instead by customizing `straight-use-package-version`.

You can disable `use-package` integration entirely by customizing
`straight-enable-use-package-integration`.

#### "Integration" with `package.el`

By default, `package.el` will automatically insert a call to
`package-initialize` into your init-file as soon as Emacs starts,
which is ridiculous. It will also do this when you perform any package
management operation. A separate system inserts some `custom` forms
into your init-file when you install a package. `straight.el` disables
all of these "features" by setting `package-enable-at-startup` to nil
and enabling some advices. You can override this behavior by
customizing `straight-enable-package-integration`, however.

To help avoid you shooting yourself in the foot by using both
`:ensure` and `:straight` at the same time in a `use-package` form
(which would cause the same package to be installed twice using two
different package managers), `straight.el` will helpfully disable
`:ensure` whenever you include `:straight` in a `use-package` form.
See [#425].

#### Integration with Flycheck

[Flycheck] sometimes creates temporary files in order to perform
syntax checking. This is a problem for `straight.el` because creation
of temporary files will cause `straight.el` to think that you have
modified a package when you actually have not. (You might ask why
`straight.el` cannot recognize temporary files and ignore them. The
answer is that for eager modification checking, all we see is that the
directory mtime for the repository has been updated, and there's no
way to disambiguate between temporary file shenanigans versus if you,
say, deleted a file.)

To work around the problem, a user option `straight-fix-flycheck` is
provided, disabled by default (for now). You can enable it *before*
loading `straight.el`, and it will work around the Flycheck problem in
the following way. When you first visit a buffer, any Flycheck checker
that involves creation of temporary files will be inhibited
automatically, although other checkers will still run. (In practice
this means no byte-compilation errors for Emacs Lisp, but you still
get Checkdoc errors.) However, after you make a change to the buffer
(by typing, etc.) then all checkers will be re-enabled. This means
that `straight.el` won't think the package was modified unless you
actually modify the buffer of a file inside it, which I think is a
reasonable compromise.

See [#508] for discussion.

#### Integration with Hydra

See [the Hydra wiki][hydra-wiki-straight-entry].

### Miscellaneous

* By default, `straight.el` explains what it is doing in the echo
  area, like this:

```
Looking for cider recipe → Cloning melpa...
```

  If your terminal does not support Unicode characters nicely, you can
  customize `straight-arrow` to display something else for the arrow.

* By default, `straight.el` reports process output the
  `*straight-process*` buffer. You can customize the name of this
  buffer via the `straight-process-buffer` user option. If you want to
  hide this buffer by default, consider adding a leading space to the
  name.

* You can prevent `straight.el` from making any modifications to the
  filesystem (though it may still read) by customizing the user option
  `straight-safe-mode` to non-nil. This may be useful for running
  tasks automatically in batch mode, to avoid multiple concurrent
  Emacs processes all making changes to the filesystem. For an example
  of how this feature may be used to safely implement asynchronous
  byte-compilation of the init-file on successful startup, see
  [Radian].

## Developer manual

This section tells you about all the interesting implementation
details and design decisions that power `straight.el` behind the
scenes. It assumes you have already read the [user manual][#user] and
the [conceptual overview][#concepts].

More to be written here in future. See [#51].

### Low-level functions

* The function `straight-chase-emulated-symlink` is provided in order
  for external packages to correctly handle the emulated symlinks
  created by `straight.el` when `straight-use-symlinks` is nil. See,
  for example, [#520].

## Trivia

This section has random, (possibly) interesting tidbits about
`straight.el` that don't fit in the other sections.

### Comments and docstrings

How did I get that statistic about the percentage of `straight.el`
that is comments and docstrings? Simple: by abusing the syntax
highlighting.

```emacs-lisp
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
```

Note that you will have to scroll through the entire buffer first,
since `font-lock-mode` computes syntax highlighting lazily.

## Contributing

Please do! Development takes place on the `develop` branch. You can
switch to that branch with

```emacs-lisp
(setq straight-repository-branch "develop")
```

and base your pull requests from it. If you have an outstanding pull
request whose features you would like to use in your configuration,
there is full support for defining `straight.el` as coming from any
branch in any fork:

```emacs-lisp
(setq straight-repository-user "my-github-username")
(setq straight-repository-branch "feat/my-cool-feature")
```

For additional information, please see [the contributor guide for my
projects](https://github.com/radian-software/contributor-guide). Note
that `straight.el` has not yet had an initial release, so you don't
have to worry about a changelog.

## FAQ
### My init time got slower

Your first step should be to customize the value of
`straight-check-for-modifications`. The best setting is `(watch-files
find-when-checking)`; this is not enabled by default because it is
impolite to spawn persistent background processes without asking, and
because you must install [Python 3][python] and
[`watchexec`][watchexec] for it to work. If you can't stand the extra
dependencies and background process, consider the setting
`(check-on-save find-when-checking)` instead, which is just as fast
but won't catch package modifications unless they are made within
Emacs via the `save-buffer` command.

Even with lazy modification detection, as described above,
`straight.el` is not quite as fast as `package.el` (by a few
percentage points). There are some planned changes which will make
`straight.el` just as fast as `package.el`, if not faster. See [#9].

### "Could not find package in recipe repositories"

Assuming that the package you're trying to install actually exists,
you need to update your recipe repositories (most likely MELPA,
possibly Emacsmirror). See the next FAQ entry. This is like running
`package-refresh-contents` under `package.el`.

Another possibility is that you are providing `straight.el` with a
feature name rather than a package name. Features are what you load
with `require` or `load`, or find in files. For example, `org-agenda`
and `org-capture` are features. Packages, on the other hand, can
provide one or more features. They are what are listed on MELPA et al.
or by `M-x straight-get-recipe`. For example, `org` and
`org-contrib` are packages.

When you write `(use-package foo ...)`, the `foo` is a *feature*, not
a package. You can give a different package name `bar` by saying
`(use-package foo :straight bar)`. And when you write
`(straight-use-package 'bar)`, the `bar` is a *package*, not a
feature.

### How do I update MELPA et al.?

Using [`M-x straight-pull-package`][#user/interactive/vc], like for
any other package. [Read more.][#user/lookup/update]

### My `use-package` form isn't working properly

There are a number of common problems you might be encountering. Check
out the following list to see if there is an easy fix.

* Make sure you're not using `:ensure` or `use-package-always-ensure`.
  Those are for `package.el` and using them with `straight.el` will
  produce weird results (namely both `package.el` and `straight.el`
  will be invoked for the same package).
* Make sure you know *both* the name of the feature and the name of
  the package. These are usually the same but not always (packages may
  provide more than one feature, ...). You give `use-package` the name
  of a *feature*, not a package (despite the name of the macro). With
  `straight-use-package-by-default` or with `:straight t`, the default
  is to try installing a package by the same name as the feature.
    * If you don't actually need to install a package, then pass
      `:straight nil` to override `straight-use-package-by-default`.
    * If the package name is different from the feature name, then
      pass `:straight <package-name>`.
* If the package or your configurations aren't being loaded, you
  probably have something wrong with your usage of `:init` and
  `:config`. By default, the behavior of `use-package` is unusably
  inconsistent. You must set either `use-package-always-defer`
  (override with `:demand t`) or `use-package-always-demand` (override
  with `:defer t`) to set a default for whether evaluating a
  `use-package` form will load the package and your configurations.
    * If you've set a package to be deferred, you then need to make
      sure there's a way for it to get loaded when needed, for example
      by means of an autoload (either provided by the package, or set
      up automatically by `use-package` via `:bind`, or set up
      manually through `use-package` via `:commands`) or by an
      explicit `require` in one of your custom commands.

### How do I uninstall a package?

My first question is: do you really need to uninstall the package?
Under `package.el`, every package on disk gets loaded into Emacs,
whether you asked for it or not. However, under `straight.el`, only
the packages you explicitly mention in your init-file get loaded into
Emacs. So the *only* problem with leaving a package on disk is that it
takes up a little bit of disk space. (But the advantage is if you
decide you want to use that package again later then you won't have to
redownload it.)

If you really want to uninstall a package, simply delete its local
repository from `~/.emacs.d/straight/repos` or run the
`straight-remove-unused-repos` command.

### The wrong version of my package was loaded

To explain this problem, let us consider a concrete example. In [this
issue][#355], a user found that the code

```emacs-lisp
(straight-use-package 'company-lsp)
(straight-use-package 'eglot)
```

sometimes resulted in runtime errors because an old version of Flymake
was being used.

The root problem here is that you want the most recent version of
Flymake to be installed by `straight.el`, but Emacs also ships an
older version, and that older version is getting loaded instead.

The older version will be loaded if `(require 'flymake)` (or similar)
is invoked before `straight.el` has made Flymake available (by means
of `(straight-use-package 'flymake)` or similar). But why would
`straight.el` not make Flymake available?

The only way that `straight.el` knows to make Flymake available is if
either you manually invoke `straight-use-package` in your init-file,
or if one of the packages that you request in your init-file declares
Flymake as a dependency. Now, any package that uses Flymake ought to
declare it as a dependency. Thus, there should be no way for a package
to load the Emacs-provided version of Flymake. However, sometimes
package authors overlook this problem (it does not always cause an
error, and sometimes package authors do not test exhaustively enough).

In this case, the problem was that `company-lsp` declared a dependency
on `lsp-mode`, and `lsp-mode` used Flymake without declaring a
dependency on `flymake`. There are two ways to work around the
problem:

* (Preferable) Fix `lsp-mode` to declare a dependency on `flymake`.
* (Workaround) Manually invoke `(straight-use-package 'flymake)`
  before `(straight-use-package 'company-lsp)`.

If you test this yourself, you might find it difficult to reproduce
the problem. That is because there is only an issue when Flymake is
actually loaded, and this doesn't necessarily happen when invoking
`(straight-use-package 'company-lsp)` *unless* `straight.el` needs to
rebuild the relevant packages (which includes byte-compilation, which
sometimes means actually loading dependencies). Keep this in mind when
testing.

This problem commonly occurs with Org, since (1) Org is popular, (2)
Emacs ships an obsolete version of Org, (3) many users want to use the
up-to-date version, and (4) Org breaks backwards compatibility
frequently. To solve it, simply make sure that you invoke
`(straight-use-package 'org)` before running any code that could load
Org, including installing any package that lists it as a dependency.

See [this issue][#236] for discussion about ways of mitigating the bad
UX of this situation.

### I get "could not read username/password" errors

This is because `straight.el` is not currently able to detect when SSH
or Git asks for your username and/or password/passphrase and then pipe
that prompt through to the minibuffer ([#334]).

To work around the problem, set up [git-credential-cache] if you use
HTTPS, and [ssh-agent] if you use SSH. That way, you won't be prompted
for your username/password. When setting up ssh-agent, be careful to
make sure that the relevant environment variables get set in Emacs.
This might be tricky since starting Emacs from the desktop (rather
than from the command line) sometimes results in it not inheriting any
environment variables from your shell.

### How do I pin package versions or use only tagged releases?

This is a planned feature. In the meantime, contributors have proposed
various workarounds. See [#246] and [#31].

`straight-x.el` now contains an experimental solution. In order to use
it you will need to add similar snippets to your Emacs configuration.

First you need to add a new profile to `straight-profiles` which also
needs to be the last profile in the list. This should be done before
you bootstrap `straight.el`.

```emacs-lisp
;; Tell straight.el about the profiles we are going to be using.
(setq straight-profiles
      '((nil . "default.el")
        ;; Packages which are pinned to a specific commit.
        (pinned . "pinned.el")))
```

After straight's install procedure you will need to add
`straight-x.el` and load the required commands.

```emacs-lisp
(autoload #'straight-x-pull-all "straight-x")
(autoload #'straight-x-freeze-versions "straight-x")
```

A variable called `straight-x-pinned-packages` has been defined in
`straight-x.el` and will contain your list of pinned packages.

From now on, you can pin a package to a specific commit like in the
following example which will pin `org-mode` to the 9.2.3 release
version:

```emacs-lisp
(let ((straight-current-profile 'pinned))
  (straight-use-package 'org)
  (straight-use-package 'org-contrib)
  ;; Pin org-mode version.
  (add-to-list 'straight-x-pinned-packages
               '("org" . "924308a150ab82014b69c46c04d1ab71e874a2e6")))
```

If you invoke `straight-x-freeze-versions` it will first write the
default lockfile and then pinned lockfile which takes precedence over
the default one if packages are thawed. `straight-x-pull-all` will
first invoke `straight-pull-all` and then restore all pinned packages.

You might want to assign the following aliases for more convenience:

```emacs-lisp
(defalias 'straight-pull-all #'straight-x-pull-all)
(defalias 'straight-freeze-versions #'straight-x-freeze-versions)
```

Please keep in mind that this is only a temporary solution and
experimental!

### How can I use the built-in version of a package?

To tell `straight.el` that you want to use the version of Org shipped
with Emacs, rather than cloning the upstream repository:

```emacs-lisp
(straight-use-package '(org :type built-in))
```

Note that `:type` is a keyword for `straight.el`, not for
`use-package`. If you are using `use-package`, then use:

```emacs-lisp
(use-package org :straight (:type built-in))
```

[Read more.][#user/recipes]

## News
### Jan 1, 2021
Breaking change: The previous behavior of the `:build` keyword is now
associated with the `:pre-build` keyword. `:build` is now used to
specify build steps (generating autoloads and texinfo, byte/native
compilation, etc). For more information on both of these keywords see
[the recipe format](#the-recipe-format).

The following customization variable names have changed:

- `straight-disable-byte-compilation` is now
  `straight-disable-compile`

- `straight-disable-native-compilation` is now
  `straight-disable-native-compile`

### April 19, 2020

Shallow clones are now compatible with lockfiles, so you can safely
set `straight-vc-git-default-clone-depth` to `1` and get massive
savings on network bandwidth and disk space.

[#principles]: #guiding-principles
[#quickstart]: #getting-started
 [#quickstart/vc]: #automatic-repository-management
[#faq]: #faq
 [#faq/package-versions]: #the-wrong-version-of-my-package-was-loaded
[#concepts]: #conceptual-overview
 [#concepts/lookup]: #where-do-recipes-come-from
 [#concepts/straight-use-package]: #what-happens-when-i-call-straight-use-package
[#comparison]: #comparison-to-other-package-managers
 [#comparison/package.el]: #comparison-to-packageel
  [#comparison/package.el/+straight.el]: #advantages-of-straightel
[#user]: #user-manual
 [#user/install]: #installing-packages-programmatically
  [#user/install/mod-detection]: #customizing-when-packages-are-built
  [#user/install/loading]: #customizing-how-packages-are-made-available
  [#user/install/hooks]: #hooks-run-by-straight-use-package
 [#user/recipes]: #the-recipe-format
 [#user/recipes/vc-backends]: #version-control-backends
 [#user/recipes/git]: #git-backend
 [#user/lookup]: #recipe-lookup
  [#user/lookup/update]: #updating-recipe-repositories
  [#user/lookup/repos]: #customizing-recipe-repositories
 [#user/overriding]: #overriding-recipes
  [#user/overriding/straight.el]: #overriding-the-recipe-for-straightel
 [#user/interactive]: #interactive-usage
  [#user/interactive/vc]: #version-control-operations
 [#user/lockfiles]: #lockfile-management
  [#user/lockfiles/profiles]: #the-profile-system
 [#user/integration]: #integration-with-other-packages
  [#user/integration/use-package]: #integration-with-use-package-1
[#dev]: #developer-manual
 [#dev/vc-backends]: #developer-manual
 [#dev/recipe-formats]: #developer-manual
 [#dev/recipe-internals]: #developer-manual
 [#dev/transactions]: #developer-manual
[#trivia]: #trivia
 [#trivia/comments]: #comments-and-docstrings
[#news]: #news

[#9]: https://github.com/radian-software/straight.el/issues/9
[#31]: https://github.com/radian-software/straight.el/issues/31
[#51]: https://github.com/radian-software/straight.el/issues/51
[#54]: https://github.com/radian-software/straight.el/issues/54
[#58]: https://github.com/radian-software/straight.el/issues/58
[#95-c1]: https://github.com/radian-software/straight.el/issues/95#issuecomment-316379495
[#110]: https://github.com/radian-software/straight.el/issues/110
[#115]: https://github.com/radian-software/straight.el/issues/115
[#119]: https://github.com/radian-software/straight.el/issues/119
[#211]: https://github.com/radian-software/straight.el/issues/211
[#236]: https://github.com/radian-software/straight.el/issues/236
[#246]: https://github.com/radian-software/straight.el/issues/246
[#323]: https://github.com/radian-software/straight.el/issues/323
[#334]: https://github.com/radian-software/straight.el/issues/334
[#355]: https://github.com/radian-software/straight.el/issues/355
[#356]: https://github.com/radian-software/straight.el/issues/356
[#357]: https://github.com/radian-software/straight.el/issues/357
[#425]: https://github.com/radian-software/straight.el/issues/425
[#437]: https://github.com/radian-software/straight.el/issues/437
[#508]: https://github.com/radian-software/straight.el/issues/508
[#520]: https://github.com/radian-software/straight.el/issues/520

[auto-compile]: https://github.com/tarsius/auto-compile
[borg]: https://github.com/emacscollective/borg
[cask]: https://github.com/cask/cask
[develop]: https://github.com/radian-software/straight.el/tree/develop
[docker]: https://www.docker.com/
[early-init-file]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html
[early-init-file-commit]: https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=24acb31c04b4048b85311d794e600ecd7ce60d3b
[el-get]: https://github.com/dimitri/el-get
[emacs]: https://www.gnu.org/software/emacs/
[emacsmirror]: https://emacsmirror.net/
[emacswiki]: https://www.emacswiki.org/
[epkg]: https://github.com/emacscollective/epkg
[epkgs]: https://github.com/emacsmirror/epkgs
[flycheck]: https://www.flycheck.org/en/latest/
[forge]: https://github.com/magit/forge
[gccemacs]: http://akrl.sdf.org/gccemacs.html
[git]: https://git-scm.com/
[git-credential-cache]: https://git-scm.com/docs/git-credential-cache
[gitter-badge]: https://badges.gitter.im/radian-software/straight.el.svg
[gitter]: https://gitter.im/radian-software/straight.el
[gnu-elpa-mirror-tool]: https://github.com/radian-software/gnu-elpa-mirror
[gnu-elpa-mirror]: https://github.com/emacs-straight
[gnu-elpa]: https://elpa.gnu.org/
[homebrew]: https://brew.sh/
[hydra-wiki-straight-entry]: https://github.com/abo-abo/hydra/wiki/straight.el
[hydra]: https://github.com/abo-abo/hydra
[issues]: https://github.com/radian-software/straight.el/issues
[keyword arguments]: https://www.emacswiki.org/emacs/KeywordArguments
[magit]: https://magit.vc/
[markdown-toc]: https://github.com/jonschlinkert/markdown-toc
[melpa-recipe-format]: https://github.com/melpa/melpa#recipe-format
[melpa]: http://melpa.org/#/
[package.el-format]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Packaging-Basics.html
[package.el]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html
[prelude]: https://github.com/bbatsov/prelude
[property-lists]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html
[python]: https://www.python.org/
[quelpa]: https://github.com/quelpa/quelpa
[radian]: https://github.com/radian-software/radian
[spacemacs]: http://spacemacs.org/
[ssh-agent]: https://www.ssh.com/ssh/agent
[symlinks-creators]: https://blogs.windows.com/buildingapps/2016/12/02/symlinks-windows-10/
[symlinks-microsoft]: https://msdn.microsoft.com/en-us/library/bb530410.aspx#vistauac_topic8
[symlinks-perforce]: https://community.perforce.com/s/article/3472
[symlinks-stackoverflow]: https://stackoverflow.com/a/29065060/3538165
[use-package]: https://github.com/jwiegley/use-package
[watchexec]: https://github.com/mattgreen/watchexec

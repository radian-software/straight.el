# straight.el

> The straightforward package manager for Emacs.

This package is a work-in-progress attempt to completely replace
`package.el` and `quelpa` with a more flexible and powerful package
management system.

## Recipe format

A `straight.el` recipe is a plist with required keys `:package` (a
string containing the name of the package) and `:repo` (a string
containing the name of the local repository the package is located
in).

The build may be customized using the `:files` key as in MELPA
recipes.

The fetcher may be specified using the `:fetcher`, `:url`, `:repo`,
and `:module` keys as in MELPA recipes, except that `:repo` has been
replaced with `:remote-repo`.

## FIXME

* Add dependency resolution

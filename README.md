el-get-cask --- [Cask][] interface on [El-Get][]
================================================

`el-get-cask` helps migration from [Cask][] to [El-Get][].  It is
actually a wrapper to interpret [Cask DSL][DSL] on top of [El-Get][].

## Installation

```
cd ~ && git clone https://github.com/tarao/el-get-cask .el-get-cask
```

## Usage

Write the following code into your emacs init file (normally
`~/.emacs.d/init.el`).

```lisp
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))


(require 'el-get-cask "~/.el-get-cask/el-get-cask.el")
(el-get-cask-load)
```

Then, place a file named `Cask` in the directory where your emacs init
file is located.  The contents of the file is Cask [DSL][].

Next time you run your Emacs, packages defined in `Cask` file are
installed.  You can also use [El-Get][] commands such as
`el-get-install` or `el-get-update` as usual.

## Directory to install

If you are not using [El-Get][] directly, all the packages are
installed in `.el-get-cask` directory under the same directory where
`Cask` file is located.  `el-get-dir` variable is overridden to
`.el-get-cask` to make [El-Get][] work.  In this case, packages
maintained by `package.el` are installed in `.el-get-cask/elpa` and
`package-user-dir` variable is overriden to this directory.

Otherwise, if you have [El-Get][] installed before loading
`el-get-cask`, then the directory settings of [El-Get][] and
`package.el` are used (they are `~/.emcas.d/el-get` and
`~/.emacs.d/elpa` by default).

## Using [El-Get][] sources

In addition to the `package.el` sources, [El-Get][] sources (recipes)
can be added by `source` DSL command as the following code.

```elisp
(source el-get)
```

If you add [El-Get][] sources, `depends-on` command installs a package
from an [El-Get][] source if available.  When there is no package
vailable in [El-Get][] recipes, `depends-on` falls back to a
`package.el` source.

## Extended notation

With `el-get-cask`, `depends-on` is just a syntactic sugar for
`el-get-bundle`.  All notation for `el-get-bundle` including package
name modifiers and an initialization form are available for
`depends-on` command.  See the documentation of `el-get-bundle` for
the detail.

## Limitations

- Only `source` and `depends-on` are supported in DSL.
- There is no CLI interface.  Use commands of [El-Get][].

[El-Get]: http://github.com/dimitri/el-get
[Cask]: http://cask.github.io/
[DSL]: http://cask.github.io/dsl.html

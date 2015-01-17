el-get-cask --- [Cask][] interface on [El-Get][]
================================================

`el-get-cask` helps migration from [Cask][] to [El-Get][].  It is
actually a wrapper to interpret [Cask DSL][DSL] by [El-Get][].

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
installed.

## [El-Get][] integration

If you want to use commands from [El-Get][], then you will need some
more configuration.  Write the following code after
`(el-get-cask-load)` in your emacs init file.

```lisp
(setq el-get-dir (expand-file-name ".el-get-cask" user-emacs-directory)
      package-user-dir (expand-file-name "elpa" el-get-dir))
```

This overrides the global configuration for `el-get.el` and
`package.el`.  Do this only if you won't use them without
`el-get-cask`.

Instead of doing this, consider translating `depends-on` in `Cask`
file into `el-get-bundle` in your Emacs init file and use [El-Get][]
without `el-get-cask`.  This is a better way to manage your packages
without harm.

## Limitations

- Only `source` and `depends-on` are supported in DSL.
- There is no CLI interface.  Use commands of [El-Get][].

[El-Get]: http://github.com/dimitri/el-get
[Cask]: http://cask.github.io/
[DSL]: http://cask.github.io/dsl.html

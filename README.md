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
installed. You can also use [El-Get][] commands such as
`el-get-install` or `el-get-update` as usual.

All the packages are installed in `.el-get-cask` directory under the
same directory where `Cask` file is located.  `el-get-dir` variable is
overridden to this directory to make [El-Get][] work.  In this case,
packages maintained by `package.el` are installed in
`.el-get-cask/elpa` and `package-user-dir` variable is overriden to
this directory.

## [El-Get][] Compatible Mode

If you have [El-Get][] installed and required before `el-get-cask` is
loaded, then `el-get-cask` works as [El-Get][] compatible mode.  In
this mode, no directory overriding for `el-get-dir` and
`package-user-dir` happen, and, the default source type is not
restricted to `elpa` in `depends-on` notation, i.e., the source type
is taken from an [El-Get][] recipe unless you explicitly specify it by
a `:type` property or a `TYPE:` modifier.

## Limitations

- Only `source` and `depends-on` are supported in DSL.
- There is no CLI interface.  Use commands of [El-Get][].

[El-Get]: http://github.com/dimitri/el-get
[Cask]: http://cask.github.io/
[DSL]: http://cask.github.io/dsl.html

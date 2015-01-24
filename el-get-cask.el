;;; el-get-cask.el --- Cask interface on El-Get

;; Author: INA Lintaro <tarao.gnn at gmail.com>
;; URL: https://github.com/tarao/el-get-cask
;; Version: 0.1
;; Package-Requires: ((el-get "5.1"))
;; Keywords: emacs package install

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'package)
(eval-when-compile (require 'cl))

(defconst el-get-cask-default-cask-file-name "Cask")

(defconst el-get-cask-default-cask-file
  (locate-user-emacs-file el-get-cask-default-cask-file-name))

(defconst el-get-cask-dir ".el-get-cask")

(defconst el-get-cask-source-alist
  '((gnu          . "http://elpa.gnu.org/packages/")
    (melpa        . "http://melpa.org/packages/")
    (melpa-stable . "http://stable.melpa.org/packages/")
    (marmalade    . "http://marmalade-repo.org/packages/")
    (SC           . "http://joseito.republika.pl/sunrise-commander/")
    (org          . "http://orgmode.org/elpa/")))

(defconst el-get-cask-fetchers
  '(:git :bzr :darcs :svn :cvs))

(defconst el-get-cask-prop-alist
  (append (mapcar #'(lambda (x) (cons x :url)) el-get-cask-fetchers)
          '((:ref . :checkout)
            (:branch . :branch))))

(defvar el-get-cask-sources nil)
(defvar el-get-cask-packages nil)
(defvar el-get-cask-el-get-source nil)

(defun el-get-cask--require-el-get ()
  (let ((el-get-dir (or (bound-and-true-p el-get-dir)
                        (locate-user-emacs-file "el-get"))))
    (add-to-list 'load-path (expand-file-name "el-get" el-get-dir)))
  (unless (require 'el-get nil 'noerror)
    (with-current-buffer
        (url-retrieve-synchronously
         "http://raw.github.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (require 'el-get-bundle))

(defun el-get-cask--dsl-func-pair (name)
  `((symbol-function ',name)
    (symbol-function ',(intern (format "el-get-cask-%s" name)))))

(defmacro el-get-cask-with-dsl (commands &rest form)
  (declare (indent 1) (debug 1))
  (let ((pairs (mapcar #'el-get-cask--dsl-func-pair commands)))
    `(cl-letf (,@pairs)
       ,@form)))

(defun el-get-cask--args (name args)
  (let (type props)
    (while (keywordp (nth 0 args))
      (let* ((kwd (nth 0 args))
             (prop (cdr (assq kwd el-get-cask-prop-alist))))
        (if (memq kwd el-get-cask-fetchers)
            (setq type (intern (substring (symbol-name kwd) 1)))
          (setq props (plist-put props (or prop kwd) (nth 1 args))))
        (setq args (cddr args))))
    (when type (setq props (plist-put props :type type)))
    (let* ((package (symbol-name name))
           (source (append (el-get-bundle-parse-name name)
                           props
                           (and el-get-cask-el-get-source
                                (ignore-errors (el-get-package-def package))))))
      (unless (plist-get source :type)
        (setq source (list* :type (el-get-bundle-guess-type source) source)))
      (setq source (append source args))
      source)))

(defun el-get-cask--elpa-initialize ()
  (unless package--initialized
    (package-initialize t))
  (unless package-archive-contents
    (package-refresh-contents)))

(defun el-get-cask--elpa-dependency (source)
  (when (and source
             (eq (plist-get source :type) 'elpa)
             (not (plist-get source :depends)))
    (let* ((name (plist-get source :name)))
      (loop for desc in (package-compute-transaction nil `((,name)))
            for package = (package-desc-name desc)
            when (not (eq package name))
            collect package))))

(defun el-get-cask--elpa-resolve-dependency (elpa-packages source-alist)
  (let (seen new-packages)
    (setq new-packages
          (loop for package in elpa-packages
                for pair = (assq package source-alist)
                for source = (cdr pair)
                for depends = (el-get-cask--elpa-dependency source)
                do (setcdr pair (plist-put source :depends depends))
                append (loop for name in depends
                             unless (or (memq name seen)
                                        (assq name source-alist))
                             collect name
                             and do (add-to-list 'seen name))))
    (setq source-alist
          (append (mapcar #'(lambda (x) (cons x `(:name ,x :type elpa)))
                          new-packages)
                  source-alist))
    (if new-packages
        (el-get-cask--elpa-resolve-dependency new-packages source-alist)
      source-alist)))

;;;###autoload
(defun el-get-cask-load (&optional file)
  "Initialize packages by evaluating DSL in FILE."
  (interactive
   (list (read-file-name "Cask file:" user-emacs-directory
                         el-get-cask-default-cask-file-name)))
  (let* ((file (or file el-get-cask-default-cask-file))
         (file (expand-file-name file))
         (base-dir (file-name-directory file))
         (el-get-installed (require 'el-get nil t))
         (el-get-cask-el-get-source nil)
         (el-get-cask-sources nil))
    (el-get-cask-with-dsl (source depends-on
                           files package package-file development)
      (load file)
      (unless el-get-installed
        (setq el-get-dir (expand-file-name el-get-cask-dir base-dir)
              package-user-dir (expand-file-name "elpa" el-get-dir)))
      (el-get-cask--require-el-get)
      (dolist (source el-get-cask-sources)
        (add-to-list 'package-archives source t))
      (let* ((alist (loop for defs in el-get-cask-packages
                          for args = (el-get-cask--args (car defs) (cdr defs))
                          collect (cons (plist-get args :name) args)))
             (names (mapcar #'car alist)))
        (when (loop for pair in alist
                    thereis (eq (plist-get (cdr pair) :type) 'elpa))
          (el-get-cask--elpa-initialize))
        (loop for pair in (el-get-cask--elpa-resolve-dependency names alist)
              do (eval `(el-get-bundle ,(car pair) ,@(cdr pair))))))))

;;;###autoload
(defmacro el-get-cask-source (name-or-alias &optional url)
  "Add a package mirror named NAME-OR-ALIAS."
  (when (stringp name-or-alias) (setq (intern name-or-alias)))
  (when (listp name-or-alias) (setq (name-or-alias (nth 1 name-or-alias))))
  (when (eq name-or-alias 'el-get) (setq el-get-cask-el-get-source t))
  `(let ((name ',name-or-alias) (url ',url))
     (let ((pair (assq name el-get-cask-source-alist)))
       (when (and (not url) pair)
         (setq name (car pair)
               url (cdr pair)))
       (add-to-list 'el-get-cask-sources (cons (symbol-name name) url) t))))

;;;###autoload
(defmacro el-get-cask-depends-on (name &rest args)
  "Add a dependency."
  (declare (indent 1) (debug 1))
  (let* ((name (if (stringp name) (intern name)
                 (or (and (listp name) (nth 1 name)) name))))
    `(add-to-list 'el-get-cask-packages '(,name ,@args) t)))

;;;###autoload
(defmacro el-get-cask-files (&rest files)
  (message "%s: not implemented" 'el-get-cask-files))

;;;###autoload
(defmacro el-get-cask-package (name version description)
  (message "%s: not implemented" 'el-get-cask-package))

;;;###autoload
(defmacro el-get-cask-package-file (filename)
  (message "%s: not implemented" 'el-get-cask-package-file))

;;;###autoload
(defmacro el-get-cask-development (&rest form)
  (message "%s: not implemented" 'el-get-cask-development))

(provide 'el-get-cask)
;;; el-get-cask.el ends here

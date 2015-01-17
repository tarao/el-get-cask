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
(require 'el-get)
(eval-when-compile (require 'cl))

(defconst el-get-cask-default-cask-file-name "Cask")

(defconst el-get-cask-default-cask-file
  (locate-user-emacs-file el-get-cask-default-cask-file-name))

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

(defun el-get-cask--args (args)
  (let ((type 'elpa) props)
    (while (keywordp (nth 0 args))
      (let* ((kwd (nth 0 args))
             (prop (cdr (assq kwd el-get-cask-prop-alist))))
        (when (memq kwd el-get-cask-fetchers)
          (setq type (intern (substring (symbol-name kwd) 1))))
        (when prop
          (setq props (plist-put props prop (nth 1 args))))
        (setq args (cddr args))))
    (list* :type type props)))

(defun el-get-cask--dsl-func-pair (name)
  `((symbol-function ',name)
    (symbol-function ',(intern (format "el-get-cask-%s" name)))))

(defmacro el-get-cask-with-dsl (commands &rest form)
  (declare (ident 1) (debug 1))
  (let ((pairs (mapcar #'el-get-cask--dsl-func-pair commands)))
    `(cl-letf (,@pairs)
       ,@form)))

;;;###autoload
(defun el-get-cask-load (&optional file)
  "Initialize packages by evaluating DSL in FILE."
  (interactive
   (list (read-file-name "Cask file:" user-emacs-directory
                         el-get-cask-default-cask-file-name)))
  (let* ((file (or file el-get-cask-default-cask-file))
         (file (expand-file-name file)))
    (el-get-cask-with-dsl (source depends-on)
      (load file)
      (dolist (source el-get-cask-sources)
        (add-to-list 'package-archives source t))
      (dolist (defs el-get-cask-packages)
        (eval `(el-get-bundle ,@defs))))))

;;;###autoload
(defmacro el-get-cask-source (name-or-alias &optional url)
  "Add a package mirror named NAME-OR-ALIAS."
  (let ((pair (assq name-or-alias el-get-cask-source-alist)))
    (when (and (not url) pair)
      (setq name-or-alias (symbol-name (car pair))
            url (cdr pair)))
    `(when (and ,name-or-alias ,url)
       (add-to-list 'el-get-cask-sources (cons ,name-or-alias ,url) t))))

;;;###autoload
(defmacro el-get-cask-depends-on (name &rest args)
  "Add a dependency."
  (let ((name (if (stringp name) (intern name)
           (or (and (listp package) (nth 1 package)) package)))
        (args (el-get-cask--args args)))
    (print (list name args))
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

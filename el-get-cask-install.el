(let ((el-get-root-dir
       (file-name-as-directory
        (or (bound-and-true-p el-get-dir)
            (expand-file-name "el-get"
                              (file-name-as-directory user-emacs-directory))))))
  ;; Install `el-get'
  (add-to-list 'load-path (expand-file-name "el-get" el-get-root-dir))
  (unless (require 'el-get nil 'noerror)
    (with-current-buffer
        (url-retrieve-synchronously
         "http://raw.github.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp)))

  ;; Install `el-get-cask'
  (let ((source '(:name el-get-cask :type github :pkgname "tarao/el-get-cask")))
    (when (bound-and-true-p el-get-cask-install-branch)
      (plist-put source :branch el-get-cask-install-branch))
    (add-to-list 'el-get-sources source)
    (el-get 'sync 'el-get-cask)))

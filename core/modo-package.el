;;; modo-package.el --- package management -*- lexical-binding: t -*-
;;; Commentary:

;; Handles package management for modo emacs.

;;; Code:

;; Prefer newer files
(setq load-prefer-newer t)
;; Add core dir to load path
(add-to-list 'load-path modo-core-dir)

;;; package.el settings
;; We're not above dirty hacks -- disable writing package-selected-packages...
(with-eval-after-load 'package
  (defun package--save-selected-packages (&rest opt) nil))
;; ...also disable writing package-initialize to init.el
(setq package--init-file-ensured t)
;; And some more benign options
(setq package-enable-at-startup nil
      package-archives nil
      package-user-dir modo-build-dir)

;;; quelpa settings
(setq quelpa-self-upgrade-p nil)
;; Don't use MELPA
(setq quelpa-update-melpa-p nil
      quelpa-checkout-melpa-p nil
      quelpa-melpa-recipe-stores nil)
;; Bootstrap using local install
(setq quelpa-ci-dir (concat modo-repo-dir "quelpa"))
(setq quelpa-dir (concat modo-cache-dir "quelpa"))
(load (expand-file-name "bootstrap.el" quelpa-ci-dir))

(defmacro modo-add-package (pkg dir)
  "Builds the package PKG from the directory DIR found in modo-repo-dir."
  `(progn
     (quelpa (quote (,pkg :fetcher file
                          :path ,(concat modo-repo-dir dir))))
     (add-to-list (quote package-selected-packages) (quote ,pkg))))

(defmacro modo-add-package-single (pkg file)
  "Builds the single-file package PKG from the file FILE found in modo-repo-dir."
  `(progn
     (quelpa (quote (,pkg :fetcher file
                          :path ,(file-name-directory (expand-file-name file modo-repo-dir))
                          :files ,(list (expand-file-name file modo-repo-dir)))))
     (add-to-list (quote package-selected-packages) (quote ,pkg))))

;; Clone from github
(defun modo-github-clone (username repo)
  "Clones the repository USERNAME/REPO from github using HTTPS."
  (let ((target (concat modo-repo-dir repo))
        (repo-url (format "https://github.com/%s/%s.git" username repo)))
    (make-directory target)
    (message "Cloning %s into %s..." repo target)
    (when (not (= 0 (shell-command (format "git clone %s %s" repo-url target)
                                   "*git clone output*")))
      (error "Failed to clone %s" repo))
    (message "Cloning %s into %s... done." repo target)))

;;; use-package
(modo-add-package-single diminish "diminish/diminish.el")
(modo-add-package-single bind-key "use-package/bind-key.el")
(modo-add-package-single use-package "use-package/use-package.el")

(setq use-package-verbose t)
(setq use-package-always-defer t)
(eval-when-compile
  (require 'use-package))
(require 'diminish)

(provide 'modo-package)
;;; modo-package.el ends here

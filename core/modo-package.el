;;; modo-package.el --- package management -*- lexical-binding: t -*-
;;; Commentary:

;; Handles package management for modo emacs.

;;; Code:

;; Prefer newer files
(setq load-prefer-newer t)
;; Add core dir and modules dir to load path
(add-to-list 'load-path modo-core-dir)
(add-to-list 'load-path modo-modules-dir)

;;; straight.el
;; Use develop branch
(setq straight-recipe-overrides
      '((modo . ((straight :type git :host github
			   :repo "raxod502/straight.el"
			   :branch "develop"
			   :files ("straight.el"))))))

;; Bootstrap snippet
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Macro for requiring modules
(defmacro modo-module (&rest modules)
  "Load all the modules listed in MODULES, with the prefix modo-.
For example, the module name ivy translates to a call to (require 'modo-ivy)."
  (let ((expansion nil)
        (module-name))
    (dolist (module modules)
      (setq module-name (format "modo-%s" (symbol-name module)))
      (push `(require ',(intern module-name)) expansion))
    (setq expansion (nreverse expansion))
    `(progn
       ,@expansion)))

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
(straight-use-package 'diminish)
(straight-use-package 'use-package)

(setq use-package-verbose t)
(setq use-package-always-defer t)
(eval-when-compile
  (require 'use-package))
(require 'diminish)

(provide 'modo-package)
;;; modo-package.el ends here

(defvar modo-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory containing the modo emacs files.")

(defvar modo-repo-dir (concat modo-emacs-dir "repos/")
  "The directory containing the modo repositories.")

;;; Core editor settings
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(blink-cursor-mode -1)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)

(setq vc-follow-symlinks t)
(column-number-mode 1)
(setq tab-width 4)
(setq sentence-end-double-space nil)
(setq mouse-yank-at-point t)

(global-auto-revert-mode 1)

;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;;; package.el
(setq package-enable-at-startup nil)
(setq package-archives nil)
(setq package-user-dir (concat modo-emacs-dir "build/"))
;; (package-initialize) is called when quelpa is bootstrapped

;;; Initialize quelpa
(setq quelpa-self-upgrade-p nil)
;; Don't use MELPA
(setq quelpa-update-melpa-p nil)
(setq quelpa-checkout-melpa-p nil)
;; Bootstrap using local install
(setq quelpa-ci-dir (concat modo-repo-dir "quelpa"))
(load (expand-file-name "bootstrap.el" quelpa-ci-dir))

(defmacro modo-add-package (pkg dir)
  "Builds the package PKG from the directory DIR found in modo-repo-dir."
  `(quelpa (quote (,pkg :fetcher file
			:path ,(concat modo-repo-dir dir)))))

(defmacro modo-add-package-single (pkg file)
  "Builds the single-file package PKG from the file FILE found in modo-repo-dir."
  `(quelpa (quote (,pkg :fetcher file
			:path ,(expand-file-name file modo-repo-dir)
			:version original))))

;;; Sunburn theme
(modo-add-package-single sunburn-theme "Sunburn-Theme/sunburn-theme.el")
(load-theme 'sunburn t)

;;; use-package
(modo-add-package-single diminish "diminish/diminish.el")
(modo-add-package-single bind-key "use-package/bind-key.el")
(modo-add-package-single use-package "use-package/use-package.el")

(setq use-package-verbose t)
(setq use-package-always-defer t)
(require 'use-package)
(require 'diminish)

;;; which-key
(modo-add-package-single which-key "emacs-which-key/which-key.el")
(use-package which-key :demand t
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.7))

;; hydra
(modo-add-package hydra "hydra")
(use-package hydra :demand t)

;;; evil mode
(modo-add-package-single undo-tree "evil/lib/undo-tree.el")
(modo-add-package-single goto-chg "evil/lib/goto-chg.el")
(modo-add-package evil "evil")
(modo-add-package-single evil-escape "evil-escape/evil-escape.el")
(use-package evil :demand t
  :init
  (setq evil-want-C-u-scroll t
	evil-want-visual-char-semi-exclusive t
	evil-want-Y-yank-to-eol t
	evil-magic t
	evil-echo-state t
	evil-indent-convert-tabs t
	evil-ex-search-vim-style-regexp t
	evil-insert-skip-empty-line t
	evil-mode-line-format 'nil
	evil-symbol-word-search t
	shift-select-mode nil)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (use-package evil-escape :demand t
    :config
    (setq-default evil-escape-key-sequence "fd")
    (evil-escape-mode 1))
  (evil-mode 1))

;;; org mode
(modo-add-package org "org-mode/lisp")
(modo-add-package-single org-bullets "org-bullets/org-bullets.el")
(use-package org
  :config
  (setq org-startup-indented t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-fontify-natively t)
  (use-package org-bullets
    :commands (org-bullets-mode)
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    (setq org-bullets-bullet-list '("•"))))

(modo-add-package-single evil-org "evil-org-mode/evil-org.el")
(use-package evil-org
  :after org
  :commands (evil-org-mode)
  :init
  (add-hook 'org-mode-hook (lambda () (evil-org-mode 1)))
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional)))

;;; elisp
(defun modo--elisp-extra-fontification ()
  "Fontify modo functions."
  (font-lock-add-keywords
   nil `(("\\(^\\|\\s-\\|,\\)(\\(\\(modo\\|\\+\\)[^) ]+\\)[) \n]" (2 font-lock-keyword-face)))))

(add-hook 'emacs-lisp-mode-hook #'modo--elisp-extra-fontification)

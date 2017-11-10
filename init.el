(defvar modo-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory containing the modo emacs files.")

(defvar modo-repo-dir (concat modo-emacs-dir "repos/")
  "The directory containing the modo repositories.")

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

;;; evil mode
(modo-add-package-single undo-tree "evil/lib/undo-tree.el")
(modo-add-package-single goto-chg "evil/lib/goto-chg.el")
(modo-add-package evil "evil")
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
  (evil-mode 1))

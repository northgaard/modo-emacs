(defvar modo-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory containing the modo emacs files.")

(defvar modo-repo-dir (concat modo-emacs-dir "repos/")
  "The directory containing the modo repositories.")

(defvar modo-cache-dir (concat modo-emacs-dir "cache/")
  "The directory storing persistent information.")
(unless (file-exists-p modo-cache-dir)
  (make-directory modo-cache-dir))

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
;; Persistent command history
(require 'savehist)
(setq savehist-file (expand-file-name "savehist" modo-cache-dir))
(savehist-mode 1)
;; Recently opened files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" modo-cache-dir)
      recentf-max-saved-items 300
      recentf-max-menu-items 15
      recentf-auto-cleanup 'never)
(recentf-mode 1)
;; Bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" modo-cache-dir)
      bookmark-save-flag 1)

;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;;; package.el
;; We're not above dirty hacks -- disable writing package-selected-packages
(with-eval-after-load 'package
  (defun package--save-selected-packages (&rest opt) nil))
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

;;; hydra
(modo-add-package hydra "hydra")
(use-package hydra :demand t)

;;; Utilities
(defun modo-find-dotfile ()
  "Opens init.el in modo-emacs-dir."
  (interactive)
  (find-file (expand-file-name "init.el" modo-emacs-dir)))

;;; general.el --- get your keybinds here!
(modo-add-package-single general "general.el/general.el")
(use-package general :demand t)

(defcustom modo-leader "SPC"
  "The general purpose leader accessible from normal mode.")

(defcustom modo-non-normal-leader "C-c"
  "Equivalent to the normal mode leader, but used in insert and emacs mode.")

(defcustom modo-major-leader ","
  "Shortcut for major mode keys, also bound to \"<leader> m\"")

;; Definer for standard shortcuts
(general-create-definer modo-define-leader-key
                        :states '(motion normal visual insert emacs)
                        :prefix modo-leader
                        :non-normal-prefix modo-non-normal-leader
                        :prefix-command 'modo-leader-command)
(general-create-definer modo-define-major-leader-key
                        :states '(motion normal visual)
                        :prefix modo-major-leader
                        :prefix-command 'modo-major-leader-command)

;; Standard keybinds
(modo-define-leader-key "f" '(:ignore t :which-key "files")
                        "fs" 'save-buffer
                        "fd" '(modo-find-dotfile :which-key "find-dotfile")
                        "b" '(:ignore t :which-key "buffers")
                        "bd" 'kill-this-buffer
                        "u" 'universal-argument
                        "m" '(modo-major-leader-command :which-key "major mode"))

(modo-define-major-leader-key "," 'evil-repeat-find-char-reverse)

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
(modo-add-package org-contribdir "org-mode/contrib/lisp")
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
    (setq org-bullets-bullet-list '("•")))
  (use-package org-drill :demand t
    :init
    (require 'cl) ;; Needs the outdated cl lib
    (setq org-id-locations-file (expand-file-name "org-id-locations" modo-cache-dir))
    (setq org-drill-add-random-noise-to-intervals-p t)
    (setq org-drill-learn-fraction 0.4)))

(modo-add-package-single evil-org "evil-org-mode/evil-org.el")
(use-package evil-org
  :after org
  :commands (evil-org-mode)
  :init
  (add-hook 'org-mode-hook (lambda () (evil-org-mode 1)))
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional)))

;;; ivy/ivy hydra/counsel/swiper
;; Ivy needs a more precise recipe because all four packages are in one repo
(quelpa `(ivy :fetcher file
              :path ,(concat modo-repo-dir "swiper")
              :files (:defaults
                      (:exclude "swiper.el" "counsel.el" "ivy-hydra.el")
                      "doc/ivy-help.org")))
(modo-add-package-single ivy-hydra "swiper/ivy-hydra.el")
(modo-add-package-single swiper "swiper/swiper.el")
(modo-add-package-single counsel "swiper/counsel.el")
(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :general
  (modo-define-leader-key "bb" 'ivy-switch-buffer
                          "r" 'ivy-resume)
  (:keymaps 'ivy-minibuffer-map
            "C-j" 'ivy-next-line
            "C-k" 'ivy-previous-line
            "M-j" 'ivy-next-history-element
            "M-k" 'ivy-previous-history-element)
  :config
  (use-package ivy-hydra
    :commands (hydra-ivy/body)))

(use-package counsel
  :general
  (:states '(normal insert visual emacs motion)
           "M-x" 'counsel-M-x)
  (modo-define-leader-key "ff" 'counsel-find-file
                          "fr" 'counsel-recentf
                          "fb" 'counsel-bookmark))

(use-package swiper
  :general
  (:states '(normal insert visual emacs motion)
           "C-s" 'swiper))

;;; elisp
(defun modo--elisp-extra-fontification ()
  "Fontify modo functions."
  (font-lock-add-keywords
   nil `(("\\(^\\|\\s-\\|,\\)(\\(\\(modo\\|\\+\\)[^) ]+\\)[) \n]" (2 font-lock-keyword-face)))))

(add-hook 'emacs-lisp-mode-hook #'modo--elisp-extra-fontification)

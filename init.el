;; -*- lexical-binding: t -*-

;;; Initial high threshold for garbage collection
(let ((normal-gc-cons-threshold (* 20 1024 1024)) ;; ~20 mb
      (init-gc-cons-threshold (* 128 1024 1024))) ;; ~128 mb
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;; Directory variables
(defvar modo-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory containing the modo emacs files.")

(defvar modo-repo-dir (concat modo-emacs-dir "repos/")
  "The directory containing the modo repositories.")

(defvar modo-cache-dir (concat modo-emacs-dir "cache/")
  "The directory storing persistent information.")
(unless (file-exists-p modo-cache-dir)
  (make-directory modo-cache-dir))

(defvar modo-temp-dir (concat modo-emacs-dir "temp/")
  "The directory storing temporary files.")
(unless (file-exists-p modo-temp-dir)
  (make-directory modo-temp-dir))

;;; UTF-8 all the things
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

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

;; Function for excluding the build and cache dirs
(defun modo-recentf-exclude-p (file)
  "A predicate which decides whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (or (string-prefix-p (file-truename modo-cache-dir) file-dir)
        (string-prefix-p (file-truename package-user-dir) file-dir)
        (string-prefix-p (file-truename quelpa-dir) file-dir))))

(add-to-list 'recentf-exclude 'modo-recentf-exclude-p)
(recentf-mode 1)
;; Bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" modo-cache-dir)
      bookmark-save-flag 1)
;; Save place in file
(setq save-place-file (expand-file-name "saveplace" modo-cache-dir))
(save-place-mode 1)
;; Better unique naming for buffers with the same base name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ;; Rename after killing buffer
(setq uniquify-ignore-buffers-re "^\\*") ;; Don't touch special buffers
;; Auto-save and backup
(setq make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-old-versions 4
      kept-new-versions 7)
(setq backup-directory-alist
      `((".*" . ,modo-temp-dir)))
(setq auto-save-list-file-name (expand-file-name "autosave-list" modo-cache-dir))
(setq auto-save-file-name-transforms
      `((".*" ,modo-temp-dir t)))

;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

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
  (let* ((dotfile (expand-file-name "init.el" modo-emacs-dir))
         (buffer-name (get-file-buffer dotfile)))
    (if buffer-name
        (switch-to-buffer buffer-name) ;; If buffer already exists, simply switch to it
      (find-file (expand-file-name "init.el" modo-emacs-dir))
      ;; Needed to make saveplace work with this function
      (run-hooks 'find-file-hook))))

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
                        "w" '(:ignore t :which-key "windows")
                        "wm" 'delete-other-windows
                        "wd" 'delete-frame
                        "wf" 'make-frame
                        "wo" 'other-frame
                        "u" 'universal-argument
                        "m" '(modo-major-leader-command :which-key "major mode"))

(modo-define-major-leader-key "," 'evil-repeat-find-char-reverse)

;;; window numbering
(modo-add-package-single window-numbering "window-numbering.el/window-numbering.el")
(use-package window-numbering :demand t
  :general
  (modo-define-leader-key "1" 'select-window-1
                          "2" 'select-window-2
                          "3" 'select-window-3
                          "4" 'select-window-4
                          "5" 'select-window-5
                          "6" 'select-window-6
                          "7" 'select-window-7
                          "8" 'select-window-8
                          "9" 'select-window-9)
  :config
  (window-numbering-mode 1))

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
  :general
  (modo-define-leader-key "ws" 'evil-window-split
                          "wv" 'evil-window-vsplit
                          "wc" 'evil-window-delete)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-ex-define-cmd "x" 'save-buffers-kill-terminal)
  (evil-ex-define-cmd "kill" 'save-buffers-kill-emacs)
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

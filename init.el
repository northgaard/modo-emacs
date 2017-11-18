;; -*- lexical-binding: t -*-
(require 'modo-core (concat user-emacs-directory "core/modo-core"))

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
(setq-default tab-width 4)
(setq sentence-end-double-space nil)
(setq mouse-yank-at-point t)
(setq load-prefer-newer t)

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(winner-mode 1)
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
        (string-prefix-p (file-truename package-user-dir) file-dir))))

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
;; Customizations for avy
(custom-set-faces
 '(avy-lead-face ((t (:background "#484349" :foreground "red" :inverse-video nil :weight bold))))
 '(avy-lead-face-0 ((t (:background "#484349" :foreground "midnight blue" :inverse-video nil :weight bold)))))

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
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.7)
  (setq which-key-sort-order #'which-key-prefix-then-key-order)
  (which-key-mode 1))

;;; hydra
(modo-add-package hydra "hydra")
(use-package hydra :demand t)

;;; Utilities
(defun modo-find-dotfile ()
  "Opens init.el in modo-emacs-dir."
  (interactive)
  (let* ((dotfile (file-truename (expand-file-name "init.el" modo-emacs-dir)))
         (buffer-name (get-file-buffer dotfile)))
    (if buffer-name
        (switch-to-buffer buffer-name) ;; If buffer already exists, simply switch to it
      (find-file dotfile)
      ;; Needed to make saveplace work with this function
      (run-hooks 'find-file-hook))))

(defun modo-delete-auto-save-file ()
  "Delete the autosave file in the currently visited buffer, if it exists."
  (interactive)
  (let ((auto-file (file-truename (make-auto-save-file-name))))
    (if (and (buffer-file-name)
             (file-exists-p auto-file))
        (delete-file auto-file)
      (message "No auto-save file exists."))))

;; Two useful functions borrowed from Steve Purcell
(defun modo-delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun modo-rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;;; evil mode
(modo-add-package-single undo-tree "evil/lib/undo-tree.el")
(use-package undo-tree :demand t
  :diminish undo-tree-mode
  :config
  (undo-tree-mode 1))
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
	evil-symbol-word-search t
	shift-select-mode nil)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-ex-define-cmd "x" 'save-buffers-kill-terminal)
  (evil-ex-define-cmd "kill" 'save-buffers-kill-emacs)
  (use-package evil-escape :demand t
    :diminish evil-escape-mode
    :config
    (setq-default evil-escape-key-sequence "fd")
    (evil-escape-mode 1))
  (evil-mode 1))

(modo-add-package-single evil-exchange "evil-exchange/evil-exchange.el")
(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-install))

(modo-add-package-single evil-snipe "evil-snipe/evil-snipe.el")
(use-package evil-snipe
  :after evil
  :diminish evil-snipe-local-mode
  :config
  (setq evil-snipe-enable-highlight nil)
  (setq evil-snipe-enable-incremental-highlight nil)
  (setq evil-snipe-repeat-keys nil)
  (setq evil-snipe-smart-case t)
  (setq evil-snipe-scope 'line)
  (setq evil-snipe-spillover-scope 'visible)
  (setq evil-snipe-repeat-scope 'buffer)
  (evil-snipe-mode 1))

(modo-add-package-single avy "avy/avy.el")
(use-package avy
  :after evil-snipe
  :config
  (setq avy-timeout-seconds 0.5)
  (setq avy-background t))

(modo-add-package-single evil-easymotion "evil-easymotion/evil-easymotion.el")
(use-package evil-easymotion
  :after avy
  :config
  (evilem-default-keybindings "C-e") ;; Not sure about this binding
  (evilem-make-motion modo-easymotion-snipe-repeat #'evil-snipe-repeat
                        :bind ((evil-snipe-scope 'buffer)
                               (evil-snipe-enable-highlight)
                               (evil-snipe-enable-incremental-highlight)))
  (evilem-make-motion modo-easymotion-snipe-repeat-reverse
                        #'evil-snipe-repeat-reverse
                        :bind ((evil-snipe-scope 'buffer)
                               (evil-snipe-enable-highlight)
                               (evil-snipe-enable-incremental-highlight)))
  (evilem-make-motion modo-easymotion-find-repeat #'evil-repeat-find-char
                      :bind ((evil-cross-lines t)))
  (evilem-make-motion modo-easymotion-find-repeat-reverse
                        #'evil-repeat-find-char-reverse
                        :bind ((evil-cross-lines t)))
  (evilem-define (kbd "C-e s") 'evil-snipe-repeat
                 :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                 :bind ((evil-snipe-scope 'buffer)
                        (evil-snipe-enable-highlight)
                        (evil-snipe-enable-incremental-highlight)))
  (evilem-define (kbd "C-e S") 'evil-snipe-repeat-reverse
                 :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                 :bind ((evil-snipe-scope 'buffer)
                        (evil-snipe-enable-highlight)
                        (evil-snipe-enable-incremental-highlight))))

(modo-add-package-single evil-surround "evil-surround/evil-surround.el")
(use-package evil-surround
  :after evil-snipe
  :config
  (global-evil-surround-mode 1))

(modo-add-package evil-commentary "evil-commentary")
(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode 1))

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
                        "f <deletechar>" '(modo-delete-this-file
                                  :which-key "delete-this-file")
                        "fn" '(modo-rename-this-file-and-buffer
                               :which-key "rename-this-file-and-buffer")
                        "f#" '(modo-delete-auto-save-file
                               :which-key "delete-auto-save-file")
                        "b" '(:ignore t :which-key "buffers")
                        "bd" 'kill-this-buffer
                        "w" '(:ignore t :which-key "windows")
                        "ws" 'evil-window-split
                        "wv" 'evil-window-vsplit
                        "wc" 'evil-window-delete
                        "wm" 'delete-other-windows
                        "wd" 'delete-frame
                        "wf" 'make-frame
                        "wo" 'other-frame
                        "wu" 'winner-undo
                        "wr" 'winner-redo
                        "u" 'universal-argument
                        "m" '(modo-major-leader-command :which-key "major mode"))

;; Horrible hackery to get the repeat behavior I want with evil-snipe
;; TODO: Figure out a clean way to do this
(defvar modo--last-evil-find 'find
  "Saves the last find type operation used, either the symbol find
or the symbol snipe.")
(make-variable-buffer-local 'modo--last-evil-find)

(defun modo--set-last-find-evil (&rest r)
  (setq modo--last-evil-find 'find))
(defun modo--set-last-find-snipe (&rest r)
  (setq modo--last-evil-find 'snipe))

(defun modo-fs-repeat (&optional count)
  (interactive "p")
  (pcase modo--last-evil-find
    (`find (evil-repeat-find-char count))
    (`snipe (evil-snipe-repeat count))))

(defun modo-fs-repeat-reverse (&optional count)
  (interactive "p")
  (pcase modo--last-evil-find
    (`find (evil-repeat-find-char-reverse count))
    (`snipe (evil-snipe-repeat-reverse count))))

(defun modo-easymotion-fs-repeat (&optional count)
  (interactive "p")
  (pcase modo--last-evil-find
    (`find (modo-easymotion-find-repeat count))
    (`snipe (modo-easymotion-snipe-repeat count))))

(defun modo-easymotion-fs-repeat-reverse (&optional count)
  (interactive "p")
  (pcase modo--last-evil-find
    (`find (modo-easymotion-find-repeat-reverse count))
    (`snipe (modo-easymotion-snipe-repeat-reverse count))))

;; Store last find type
(advice-add #'evil-find-char :after #'modo--set-last-find-evil)
(advice-add #'evil-find-char-backward :after #'modo--set-last-find-evil)
(advice-add #'evil-find-char-to :after #'modo--set-last-find-evil)
(advice-add #'evil-find-char-to-backward :after #'modo--set-last-find-evil)
(advice-add #'evil-snipe-s :after #'modo--set-last-find-snipe)
(advice-add #'evil-snipe-S :after #'modo--set-last-find-snipe)
(advice-add #'evil-snipe-x :after #'modo--set-last-find-snipe)
(advice-add #'evil-snipe-X :after #'modo--set-last-find-snipe)

(setq evil-snipe-parent-transient-map
      (let ((map (make-sparse-keymap)))
        (define-key map ";" #'evil-snipe-repeat)
        (define-key map "," #'modo-major-leader-command)
        map))
(modo-define-major-leader-key "," #'modo-fs-repeat-reverse)
(general-define-key :states '(motion normal visual)
                    ";" #'modo-fs-repeat
                    "g;" #'modo-easymotion-fs-repeat
                    "g," #'modo-easymotion-fs-repeat-reverse
                    "g." #'goto-last-change
                    "g:" #'goto-last-change-reverse)

;; Info-mode
(general-define-key :states 'motion
                    :keymaps 'Info-mode-map
                    "SPC" nil ;; Get back leader
                    "j" 'Info-scroll-up
                    "k" 'Info-scroll-down
                    "h" 'Info-history-back
                    "l" 'Info-history-forward
                    "C-j" 'evil-next-line
                    "C-k" 'evil-previous-line
                    "C-h" 'evil-backward-char
                    "C-l" 'evil-forward-char
                    "gg" 'evil-goto-first-line
                    "G" 'evil-goto-line
                    "s" 'swiper)

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

;;; Fonts
(modo-add-package-single default-text-scale "default-text-scale/default-text-scale.el")
(use-package default-text-scale
  :init
  (defhydra hydra-font-size (:color red)
    "Change font size"
    ("+" default-text-scale-increase "increase")
    ("M-+" text-scale-increase "increase (buffer)")
    ("-" default-text-scale-decrease "decrease")
    ("M--" text-scale-decrease "decrease (buffer)")
    ("q" nil "quit"))
  :general
  (modo-define-leader-key "z" '(hydra-font-size/body :which-key "font-size")))

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
(add-to-list 'package-selected-packages 'ivy)
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
  :commands (swiper)
  :general
  (:states '(normal insert visual emacs motion)
           "C-s" 'swiper))

;;; elisp
(defun modo--elisp-extra-fontification ()
  "Fontify modo functions."
  (font-lock-add-keywords
   nil `(("\\(^\\|\\s-\\|,\\)(\\(\\(modo\\|\\+\\)[^) ]+\\)[) \n]" (2 font-lock-keyword-face)))))

(add-hook 'emacs-lisp-mode-hook #'modo--elisp-extra-fontification)

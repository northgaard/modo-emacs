;;; modo-editor.el --- editor configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Core customization for pleasant editing.

;;; Code:

;;; Base settings
(setq-default indent-tabs-mode nil
              tab-width 4)
(setq vc-follow-symlinks t
      create-lockfiles nil
      sentence-end-double-space nil
      mouse-yank-at-point t)
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;;; Persistent command history
(use-package savehist
  :demand t
  :config
  (setq savehist-file (expand-file-name "savehist" modo-cache-dir))
  (savehist-mode 1))

;;; Recently opened files
;; Function for excluding the build and cache dirs
(defun modo-recentf-exclude-p (file)
  "A predicate which decides whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (or (string-prefix-p (file-truename modo-cache-dir) file-dir)
        (string-prefix-p (file-truename package-user-dir) file-dir))))

(use-package recentf
  :demand t
  :config
  (setq recentf-save-file (expand-file-name "recentf" modo-cache-dir)
        recentf-max-saved-items 300
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (add-to-list 'recentf-exclude 'modo-recentf-exclude-p)
  (recentf-mode 1))

;;; Bookmarks
(use-package bookmark
  :demand t
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" modo-cache-dir)
        bookmark-save-flag 1))

;;; Save place in file
(setq save-place-file (expand-file-name "saveplace" modo-cache-dir))
(save-place-mode 1)

;;; Better unique naming for buffers with the same base name
(use-package uniquify
  :demand t
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t ;; Rename after killing buffer
        uniquify-ignore-buffers-re "^\\*")) ;; Don't touch special buffers

;;; Auto-save and backup
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

;;; undo-tree
(straight-use-package 'undo-tree)
(use-package undo-tree
  :demand t
  :diminish undo-tree-mode
  :config
  (undo-tree-mode 1))

;;; ag from emacs
(straight-use-package 'ag)
(use-package ag)

;;; emacs server
(use-package server
  :demand t
  :init
  (setq server-auth-dir (concat modo-cache-dir "server/"))
  :config
  (unless (eq (server-running-p) t)
    (server-start)))

;;; eshell
(use-package eshell
  :init
  (setq eshell-directory-name (concat modo-cache-dir "eshell/")))

;;; Compilation
(use-package compile
  :config
  (setq compilation-scroll-output 'first-error))

;;; ediff
(use-package ediff
  :config
  (setq ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-diff-options "-w"))

(provide 'modo-editor)
;;; modo-editor.el ends here

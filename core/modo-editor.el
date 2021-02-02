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
      mouse-yank-at-point t
      save-interprogram-paste-before-kill t
      disabled-command-function nil
      confirm-kill-processes nil)

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
        (string-prefix-p (file-truename modo-temp-dir) file-dir))))

(use-package recentf
  :demand t
  :config
  (setq recentf-save-file (expand-file-name "recentf" modo-cache-dir)
        recentf-max-saved-items 50
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

;;; ediff
(use-package ediff
  :config
  (setq ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-diff-options "-w"))

;;; smerge
(use-package smerge-mode
  :after hydra
  :config
  (defhydra smerge-hydra
    (:color pink
            :hint nil
            :pre (if (not smerge-mode) (smerge-mode 1))
            ;; Disable smerge-mode if no conflicts remain
            :post (smerge-auto-leave)) 
    "
^Move^          ^Keep^               ^Diff^                 ^Other^
^^--------------^^-------------------^^---------------------^^-------
_j_: next       _b_ase               _<_: upper/base        _C_ombine
_k_: prev       _u_pper              _=_: upper/lower       _r_esolve
_C-j_: up       _l_ower              _>_: base/lower        _K_ill current
_C-k_: down     _a_ll                _R_efine
^^              _RET_: current       _E_diff
"
    ("C-j" next-line)
    ("C-k" previous-line)
    ("j" smerge-next)
    ("k" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("K" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue)))

(provide 'modo-editor)
;;; modo-editor.el ends here

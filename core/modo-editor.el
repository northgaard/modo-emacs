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
;; These settings should speed up redisplay, but will break rendering
;; of right-to-left languages. This is not an issue for me personally,
;; but caveat emptor.
(setq-default bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
;; Mitigate performance issues with files with long lines
(global-so-long-mode 1)

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
      auto-save-default t
      vc-make-backup-files t
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

(defun modo-disable-auto-save-backup-locally ()
  "Disables auto saving and backups in the local buffer."
  (setq-local make-backup-files nil)
  (setq-local auto-save-default nil))

(straight-use-package '(ws-butler :type git :host github :repo "hlissner/ws-butler"))
(use-package ws-butler
  :demand t
  :config
  (setq ws-butler-keep-whitespace-before-point nil)
  (ws-butler-global-mode 1))

(straight-use-package 'sudo-edit)
(use-package sudo-edit
  :general
  (modo-define-leader-key :keymaps 'override
    "fu" 'sudo-edit
    "fU" 'sudo-edit-find-file)
  :config
  (sudo-edit-indicator-mode 1))

(provide 'modo-editor)
;;; modo-editor.el ends here

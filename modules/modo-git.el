;;; modo-git.el --- it's magit, baby! -*- lexical-binding: t -*-
;;; Commentary:

;; Emacs interface to git based on magit.

;;; Code:

(straight-use-package 'gitconfig-mode)
(use-package gitconfig-mode)

(straight-use-package 'gitignore-mode)
(use-package gitignore-mode)

(straight-use-package 'magit)
(use-package magit
  :commands (magit-status magit-blame)
  ;; Start in insert mode for commit messages
  :hook ((with-editor-mode . evil-insert-state))
  :general
  (modo-define-leader-key "s" 'magit-status)
  :config
  (when (string= "SPC" modo-leader)
    (general-define-key :keymaps 'magit-mode-map
                        modo-leader nil)))

(straight-use-package 'evil-magit)
(use-package evil-magit
  :demand t
  :after magit
  :init
  (setq evil-magit-want-horizontal-movement t))

(provide 'modo-git)
;;; modo-git.el ends here

;;; modo-git.el --- it's magit, baby! -*- lexical-binding: t -*-
;;; Commentary:

;; Emacs interface to git based on magit.

;;; Code:

(straight-use-package 'gitconfig-mode)
(use-package gitconfig-mode)

(straight-use-package 'gitignore-mode)
(use-package gitignore-mode)

(defvar modo--git-abbrev-length (let ((abbrev (shell-command-to-string
                                               "git config core.abbrev")))
                                  (if (string-empty-p abbrev)
                                      "7" ;; Default value
                                    abbrev))
  "The amount of characters git minimally uses for abbreviated hashes.")

(straight-use-package 'magit)
(use-package magit
  :commands (magit-status magit-blame)
  ;; Start in insert mode for commit messages
  :hook ((git-commit-setup . evil-normalize-keymaps)
         (git-commit-setup . evil-insert-state))
  :general
  (modo-define-leader-key "s" 'magit-status)
  ;; Let's pretend with-editor-mode is a major mode
  (modo-define-major-leader-key :keymaps 'with-editor-mode-map
    "c" 'with-editor-finish
    "q" 'with-editor-cancel)
  :config
  (defun avy-magit-log-goto-commit ()
    "Avy jump to an arbitrary commit in the magit-log view."
    (interactive)
    (avy--generic-jump (format "^[a-zA-Z0-9]\\{%s,\\} [*|] " modo--git-abbrev-length)
                       nil 'at-full))
  (when (string= "SPC" modo-leader)
    (general-define-key :keymaps 'magit-mode-map
                        modo-leader nil))
  ;; Use projectile to get known repositories
  (setq magit-repository-directories
        (mapcar (lambda (dir)
                  (cons (directory-file-name dir) 0))
                (cl-remove-if-not (lambda (project)
                                    (file-directory-p (concat project "/.git/")))
                                  (projectile-relevant-known-projects)))))

(straight-use-package 'evil-magit)
(use-package evil-magit
  :demand t
  :after magit
  :general
  (:states '(motion normal visual)
           :keymaps 'magit-log-mode-map
           "gs" #'avy-magit-log-goto-commit)
  :init
  (setq evil-magit-want-horizontal-movement t))

(provide 'modo-git)
;;; modo-git.el ends here

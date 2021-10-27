;;; modo-projects.el --- project handling -*- lexical-binding: t -*-
;;; Commentary:

;; Handling of projects powered by projectile.

;;; Code:

(straight-use-package 'projectile)
(use-package projectile
  :demand t
  :diminish projectile-mode
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-cache-file (concat modo-cache-dir "projectile.cache")
        projectile-enable-caching t
        projectile-require-project-root nil
        projectile-known-projects-file (concat modo-cache-dir "projectile.projects")
        projectile-use-git-grep t
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-indexing-method 'hybrid
        projectile-sort-order 'recentf
        projectile-per-project-compilation-buffer t)
  ;; Get rid of the default mappings
  (setq projectile-mode-map (make-sparse-keymap))
  :general
  (modo-define-leader-key :keymaps 'override
    "p" '(projectile-command-map :wk "projectile")))

(evil-define-command projectile-grep-ex-command (prompt)
  "Grep in the current project with an ex query."
  (interactive "<a>")
  (projectile-grep prompt))
(evil-ex-define-cmd "pgrep" #'projectile-grep-ex-command)

(provide 'modo-projects)
;;; modo-projects.el ends here

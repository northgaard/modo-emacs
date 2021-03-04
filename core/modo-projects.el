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
        projectile-sort-order 'recentf)
  ;; Get rid of the default mappings
  (setq projectile-mode-map (make-sparse-keymap))
  :general
  (modo-define-leader-key "p" '(projectile-command-map :wk "projectile")))

(provide 'modo-projects)
;;; modo-projects.el ends here

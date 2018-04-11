;;; modo-projects.el --- project handling -*- lexical-binding: t -*-
;;; Commentary:

;; Handling of projects powered by projectile.

;;; Code:

(straight-use-package 'projectile)
(use-package projectile
  :demand t
  :diminish projectile-mode
  :init
  (setq projectile-cache-file (concat modo-cache-dir "projectile.cache")
        projectile-enable-caching t
        projectile-require-project-root nil
        projectile-known-projects-file (concat modo-cache-dir "projectile.projects")
        projectile-indexing-method 'alien)
  (add-hook 'after-init-hook #'projectile-mode)
  ;; Get rid of the default mappings
  (setq projectile-mode-map (make-sparse-keymap))
  :general
  (modo-define-leader-key "p" '(projectile-command-map :which-key "projectile")))

(provide 'modo-projects)
;;; modo-projects.el ends here

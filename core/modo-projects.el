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
    "p" '(projectile-command-map :wk "projectile"))
  :config
  ;; The `project' equivalent for find-file and switch-buffer are
  ;; nicer than the projectile equivalents, because they have better
  ;; support for annotations and completion categories. Luckily, we
  ;; can use projectile to provide the completions, and get the best
  ;; of both worlds.
  (require 'project)
  (setq project-list-file (concat modo-cache-dir "projects"))
  (push (defun modo-project-projectile (dir)
          (when-let ((root (projectile-project-root dir)))
            (cons 'projectile root)))
        project-find-functions)
  (cl-defmethod project-root ((project (head projectile)))
    (cdr project))
  (cl-defmethod project-files ((project (head projectile)) &optional _dirs)
    (let ((root (project-root project)))
      ;; Make paths absolute and ignore the optional dirs argument, see
      ;; https://github.com/bbatsov/projectile/issues/1591#issuecomment-896423965
      (mapcar (apply-partially #'concat root)
              (projectile-project-files root))))
  (cl-defmethod project-buffers ((project (head projectile)))
    (projectile-project-buffers (project-root project)))
  (general-define-key :keymaps 'override
                      [remap projectile-find-file] 'project-find-file
                      [remap projectile-switch-to-buffer] 'project-switch-to-buffer))

(evil-define-command projectile-grep-ex-command (prompt)
  "Grep in the current project with an ex query."
  (interactive "<a>")
  (projectile-grep prompt))
(evil-ex-define-cmd "pgrep" #'projectile-grep-ex-command)

(provide 'modo-projects)
;;; modo-projects.el ends here

;;; modo-git.el --- it's magit, baby! -*- lexical-binding: t -*-
;;; Commentary:

;; Emacs interface to git based on magit.

;;; Code:

(straight-use-package 'gitconfig-mode)
(use-package gitconfig-mode)

(straight-use-package 'gitignore-mode)
(use-package gitignore-mode)

(straight-use-package 'gitattributes-mode)
(use-package gitattributes-mode)

(modo-deflazy modo--git-abbrev-length
              (let ((abbrev (shell-command-to-string
                             "git config core.abbrev")))
                (if (string-empty-p abbrev)
                    "7" ;; Default value
                  abbrev))
              "The amount of characters git minimally uses for abbreviated hashes.")

(defun modo--activate-smerge-hydra ()
  (when smerge-mode
    (smerge-hydra/body)))

(straight-use-package 'transient)
(use-package transient
  :config
  (setq transient-history-file (concat modo-cache-dir "transient/history.el")
        transient-levels-file (concat modo-cache-dir "transient/levels.el")
        transient-values-file (concat modo-cache-dir "transient/values.el")))

(straight-use-package 'magit)
(use-package magit
  ;; Start in insert mode for commit messages
  :hook ((git-commit-setup . evil-normalize-keymaps)
         (git-commit-setup . evil-insert-state)
         (magit-diff-visit-file . modo--activate-smerge-hydra))
  :general
  (modo-define-leader-key "s" 'magit-status)
  ;; Let's pretend with-editor-mode is a major mode
  (modo-define-major-leader-key :keymaps 'with-editor-mode-map
    "c" 'with-editor-finish
    "q" 'with-editor-cancel)
  (modo-define-leader-key
    :keymaps 'override
    "v" '(:ignore t :wk "version control")
    "vb" 'magit-blame
    "vf" 'magit-find-file
    "vd" 'magit-dispatch
    "vD" 'magit-file-dispatch)
  :config
  ;; Lazy initialization
  (defun avy-magit-log-goto-commit ()
    "Avy jump to an arbitrary commit in the magit-log view."
    (interactive)
    (avy-jump (format "^[a-zA-Z0-9]\\{%s,\\} [*|] "
                      (modo--git-abbrev-length-value))))
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

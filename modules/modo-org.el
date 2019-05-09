;;; modo-org.el --- org-mode -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration of org-mode for modo emacs.

;;; Code:

(defcustom modo-org-root-dir "~/org"
  "Directory for org files, typically stored in some shared folder,
i.e. with Dropbox.")

(defun modo-get-org-file (file)
  "Returns the full path to org file FILE in `modo-org-root-dir'."
  (expand-file-name file modo-org-root-dir))

(defun modo-org-mode-setup ()
  (org-bullets-mode 1)
  (evil-org-mode 1)
  (evil-normalize-keymaps))

(defun modo-find-org-file (filename)
  "Find file FILENAME, using `modo-org-root-dir' as the base
directory for completion."
  (interactive
   (list (read-file-name "Select file: "
                         modo-org-root-dir
                         (file-name-nondirectory
                          (modo-get-org-file "gtd.org")))))
   (find-file filename)
   (run-hooks 'find-file-hook))

(straight-use-package 'git)
(straight-use-package 'org-plus-contrib)
(use-package org
  :hook (org-mode . modo-org-mode-setup)
  :general
  (modo-define-leader-key :keymaps 'override
    "a" 'org-agenda
    "c" 'org-capture
    "fo" '(modo-find-org-file :wk "find-org-file"))
  :init
  ;; Make double extra sure that the built-in org-version is not loaded
  (when (featurep 'org-version)
    (unload-feature 'org-version t))
  ;; Now load the fix
  (require 'org-version (concat modo-modules-dir "org-version-fix"))
  :config
  (setq org-startup-indented t
        org-src-tab-acts-natively t
        org-src-fontify-natively t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)"
                    "|" "DONE(d)" "CANCELED(c)")))
  (setq org-agenda-files (mapcar #'modo-get-org-file
                                 '("inbox.org" "gtd.org" "tickler.org")))
  (setq org-capture-templates `(("t" "Todo [inbox]" entry
                                 (file+headline ,(modo-get-org-file "inbox.org") "Tasks")
                                 "* TODO %?")
                                ("T" "Tickler" entry
                                 (file+headline ,(modo-get-org-file "tickler.org") "Tickler")
                                 "* %? \n %U")))
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets `((,(modo-get-org-file "gtd.org") :maxlevel . 3)
                             (,(modo-get-org-file "someday.org") :level . 1)
                             (,(modo-get-org-file "tickler.org") :maxlevel . 2))))

(straight-use-package 'org-bullets)
(use-package org-bullets
  :commands (org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("•")))

(straight-use-package 'evil-org)
(use-package evil-org
  :commands (evil-org-mode)
  :custom (evil-org-key-theme '(navigation insert textobjects additional))
  :config
  (evil-org-set-key-theme)
  (defun avy-org-goto-header ()
    "Jump to an org header at any level."
    (interactive)
    (avy--generic-jump "^\\*+ " nil 'pre))
  (general-define-key :states '(motion normal visual)
                      :keymaps 'evil-org-mode-map
                      "gs" #'avy-org-goto-header
                      "gS" #'avy-org-goto-heading-timer))

(provide 'modo-org)
;;; modo-org.el ends here

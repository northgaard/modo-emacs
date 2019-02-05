;;; modo-org.el --- org-mode -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration of org-mode for modo emacs.

;;; Code:

(defun modo-org-mode-setup ()
  (org-bullets-mode 1)
  (evil-org-mode 1)
  (evil-normalize-keymaps))

(straight-use-package 'git)
(straight-use-package 'org-plus-contrib)
(use-package org
  :hook (org-mode . modo-org-mode-setup)
  :init
  ;; Make double extra sure that the built-in org-version is not loaded
  (when (featurep 'org-version)
    (unload-feature 'org-version t))
  ;; Now load the fix
  (require 'org-version (concat modo-modules-dir "org-version-fix"))
  :config
  (setq org-startup-indented t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-fontify-natively t))

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

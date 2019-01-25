;;; modo-org.el --- org-mode -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration of org-mode for modo emacs.

;;; Code:

(straight-use-package 'git)
(straight-use-package 'org-plus-contrib)
(use-package org
  :config
  (defun modo-org-mode-setup-hook ()
    (org-bullets-mode 1)
    (evil-org-mode 1))
  ;; Make double extra sure that the built-in org-version is not loaded
  (when (featurep 'org-version)
    (unload-feature 'org-version t))
  ;; Now load the fix
  (require 'org-version (concat modo-modules-dir "org-version-fix"))
  (setq org-startup-indented t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-fontify-natively t)
  (add-hook 'org-mode-hook #'modo-org-mode-setup-hook)
  (use-package org-drill
    :demand t
    :init
    (require 'cl) ;; Needs the outdated cl lib
    (setq org-id-locations-file (expand-file-name "org-id-locations" modo-cache-dir))
    (setq org-drill-add-random-noise-to-intervals-p t)
    (setq org-drill-learn-fraction 0.4)))

(straight-use-package 'org-bullets)
(use-package org-bullets
  :after org
  :commands (org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("•")))

(straight-use-package 'evil-org)
(use-package evil-org
  :after org
  :commands (evil-org-mode)
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional))
  (evilem-make-motion modo-avy-org-goto-header #'org-previous-visible-heading
                      :initial-point 'point-max)
  (general-define-key :states '(motion normal visual)
                      :keymaps 'evil-org-mode-map
                      :definer 'minor-mode
                      "gs" #'modo-avy-org-goto-header
                      "gS" #'avy-org-goto-heading-timer))

(provide 'modo-org)
;;; modo-org.el ends here

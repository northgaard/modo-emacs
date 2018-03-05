;;; modo-org.el --- org-mode -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration of org-mode for modo emacs.

;;; Code:

(straight-use-package 'git)
(straight-use-package 'org-plus-contrib)
(use-package org
  :config
  ;; Make double extra sure that the built-in org-version is not loaded
  (unload-feature 'org-version t)
  ;; Now load the fix
  (require 'org-version (concat modo-modules-dir "org-version-fix"))
  (setq org-startup-indented t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-fontify-natively t)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (add-hook 'org-mode-hook (lambda () (evil-org-mode 1)))
  (use-package org-drill :demand t
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
  (evil-org-set-key-theme '(navigation insert textobjects additional)))

(provide 'modo-org)
;;; modo-org.el ends here

;;; modo-org.el --- org-mode -*- lexical-binding: t -*-
;;; Commentary

;; Configuration of org-mode for modo emacs.

;;; Code:

(straight-use-package 'org-plus-contrib)
(use-package org
  :config
  (setq org-startup-indented t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-fontify-natively t)
  (use-package org-drill :demand t
    :init
    (require 'cl) ;; Needs the outdated cl lib
    (setq org-id-locations-file (expand-file-name "org-id-locations" modo-cache-dir))
    (setq org-drill-add-random-noise-to-intervals-p t)
    (setq org-drill-learn-fraction 0.4)))

(straight-use-package 'org-bullets)
(use-package org-bullets
  :after org
  :config
  (setq org-bullets-bullet-list '("•"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(straight-use-package 'evil-org)
(use-package evil-org
  :after org
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional))
  (add-hook 'org-mode-hook (lambda () (evil-org-mode 1))))

(provide 'modo-org)
;;; modo-org.el ends here

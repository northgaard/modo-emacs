;;; modo-treemacs.el --- treemacs file browser -*- lexical-binding: t -*-
;;; Commentary:

;; Sometimes a visual overview is kind of nice.

;;; Code:

(straight-use-package 'treemacs)
(use-package treemacs
  :general
  ("C-<tab>" 'treemacs)
  :init
  (setq treemacs-persist-file
        (expand-file-name
         "treemacs-persist" modo-cache-dir)
        treemacs-last-error-persist-file
        (expand-file-name
         "treemacs-persist-at-last-error" modo-cache-dir)))

(straight-use-package 'treemacs-evil)
(use-package treemacs-evil
  :after treemacs
  :demand t)

(provide 'modo-treemacs)
;;; modo-treemacs.el ends here

;;; modo-company.el --- company-based completion -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration of company mode for insertion completion.

;;; Code:
(straight-use-package 'company)
(use-package company
  :commands (company-mode
             global-company-mode
             company-complete
             company-complete-common)
  :general
  (:states '(insert emacs)
           "C-SPC" 'company-complete)
  (:keymaps 'company-active-map
            "C-j" 'company-select-next
            "C-k" 'company-select-previous
            "C-o" 'counsel-company
            [tab] 'company-complete-common-or-cycle)
  :config
  (make-variable-buffer-local 'company-backends)
  (setq company-idle-delay nil
        company-tooltip-align-annotations t
        company-selection-wrap-around t
        company-require-match 'never
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)
        company-transformers '(company-sort-by-occurrence))
  (setq-default company-backends '(company-capf company-dabbrev company-ispell))
  (global-company-mode 1))

(straight-use-package 'company-statistics)
(use-package company-statistics
  :after company
  :config
  (setq company-statistics-file (expand-file-name "company-stats-cache.el"
                                                  modo-cache-dir))
  (company-statistics-mode 1))

(provide 'modo-company)
;;; modo-company.el ends here

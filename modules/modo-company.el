;;; modo-company.el --- company-based completion -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration of company mode for insertion completion.

;;; Code:
(straight-use-package 'company)
(use-package company
  :commands (company-mode
             global-company-mode
             company-complete
             company-complete-common
             company-yasnippet)
  :general
  (:states '(insert emacs)
           "C-SPC" 'company-complete)
  (:states '(insert)
          "C-y" 'company-yasnippet)
  (:keymaps 'company-active-map
            "C-j" 'company-select-next
            "C-k" 'company-select-previous
            "C-o" 'counsel-company
            [tab] 'company-complete-common-or-cycle)
  :config
  (setq company-idle-delay nil
        company-tooltip-align-annotations t
        company-selection-wrap-around t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))
  (global-company-mode 1))

(straight-use-package 'company-prescient)
(use-package company-prescient
  :demand t
  :after company
  :config
  (setq prescient-save-file (concat modo-cache-dir "prescient-save.el"))
  (company-prescient-mode 1))

(provide 'modo-company)
;;; modo-company.el ends here

;;; modo-company.el --- company-based completion -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration of company mode for insertion completion.

;;; Code:

(defun company-orderless-just-one-face (fn &rest args)
  (let ((orderless-match-faces [completions-common-part]))
    (apply fn args)))

(straight-use-package 'company)
(straight-use-package 'consult-company)
(use-package company
  :defer 5
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
            "C-h" 'help-command
            "M-h" 'company-show-doc-buffer
            "C-d" 'company-next-page
            "C-b" 'company-previous-page
            "C-o" 'consult-company
            "C-w" 'evil-delete-backward-word
            "C-l" 'company-show-location
            "<tab>" 'company-complete-common-or-cycle
            "TAB" 'company-complete-common-or-cycle)
  :config
  (setq company-idle-delay nil
        company-tooltip-align-annotations t
        company-selection-wrap-around t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-format-margin-function nil
        company-show-quick-access 'left
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))
  (advice-add 'company-capf--candidates :around #'company-orderless-just-one-face)
  ;; Ensure that company-emulation-alist is the first item of
  ;; emulation-mode-map-alists, and thus higher priority than the
  ;; keymaps of evil mode.
  (modo-add-hook (evil-local-mode-hook
                  :name "modo--ensure-company-emulation")
    (when (memq 'company-emulation-alist emulation-mode-map-alists)
      (company-ensure-emulation-alist)))
  (global-company-mode 1))

(provide 'modo-company)
;;; modo-company.el ends here

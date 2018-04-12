;;; modo-csharp.el --- defying Visual Studio -*- lexical-binding: t -*-
;;; Commentary:

;; Making C# coding in Emacs doable.

;;; Code:

(straight-use-package 'csharp-mode)
(use-package csharp-mode)

(straight-use-package 'omnisharp)
(use-package omnisharp
  :demand t
  :diminish omnisharp-mode
  :after csharp-mode
  :hook ((csharp-mode . flycheck-mode)
         (csharp-mode . omnisharp-mode)
         (omnisharp-mode . modo-omnisharp-setup-hook))
  :init
  (setq omnisharp-autocomplete-want-documentation nil
        omnisharp-cache-directory (concat modo-cache-dir "omnisharp"))
  :config
  (defun modo-omnisharp-setup-hook ()
    (require 'company)
    (add-to-list 'company-backends #'company-omnisharp)
    (setq-local company-idle-delay 0.3)
    (setq-local company-minimum-prefix-length 3)
    (add-hook 'before-save-hook #'omnisharp-code-format-entire-file nil t)))

(provide 'modo-csharp)
;;; modo-csharp.el ends here

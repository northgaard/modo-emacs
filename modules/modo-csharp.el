;;; modo-csharp.el --- defying Visual Studio -*- lexical-binding: t -*-
;;; Commentary:

;; Making C# coding in Emacs doable.

;;; Code:

(straight-use-package 'csharp-mode)
(use-package csharp-mode)

(straight-use-package 'omnisharp)
(use-package omnisharp
  :demand t
  :after csharp-mode
  :hook ((csharp-mode . flycheck-mode)
         (csharp-mode . omnisharp-mode))
  :init
  (setq omnisharp-autocomplete-want-documentation nil
        omnisharp-cache-directory (concat modo-cache-dir "omnisharp"))
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends #'company-omnisharp)))
                           

(provide 'modo-csharp)
;;; modo-csharp.el ends here

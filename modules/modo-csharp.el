;;; modo-csharp.el --- defying Visual Studio -*- lexical-binding: t -*-
;;; Commentary:

;; Making C# coding in Emacs doable.

;;; Code:
(defcustom modo-omnisharp-dir "c:/OmniSharp/"
  "Directory where Omnisharp is installed.")

(straight-use-package 'csharp-mode)
(use-package csharp-mode)

(straight-use-package 'omnisharp)
(use-package omnisharp
  :demand t
  :after csharp-mode
  :preface
  (setq omnisharp-autocomplete-want-documentation nil
        omnisharp-server-executable-path (concat modo-omnisharp-dir "OmniSharp.exe"))
  :config
  (add-hook 'csharp-mode (lambda () 
                           (flycheck-mode 1)
                           (omnisharp-mode 1)))
  (with-eval-after-load 'company
    (add-to-list 'company-backends #'company-omnisharp)))
                           

(provide 'modo-csharp)
;;; modo-csharp.el ends here

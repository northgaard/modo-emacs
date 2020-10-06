;;; modo-csharp.el --- defying Visual Studio -*- lexical-binding: t -*-
;;; Commentary:

;; Making C# coding in Emacs doable.

;;; Code:

(defun modo-omnisharp-setup-hook ()
  (require 'company)
  (add-to-list 'company-backends #'company-omnisharp)
  (setq-local company-idle-delay 0.3)
  (setq-local company-minimum-prefix-length 3)
  (setq-local require-final-newline nil)
  ;; Activate eldoc mode for documentation
  (eldoc-mode 1))

(defun omnisharp-add-dot-and-company-complete ()
  (interactive)
  (insert ".")
  (company-complete))

(straight-use-package 'csharp-mode)
(use-package csharp-mode
  :hook ((csharp-mode . flycheck-mode)
         (csharp-mode . omnisharp-mode))
  :init
  (add-to-list 'projectile-other-file-alist '("xaml" "xaml.cs"))
  (add-to-list 'projectile-other-file-alist '("xaml.cs" "xaml"))
  (when modo-ligatures-active
    (ligature-set-ligatures 'csharp-mode '(".?" "??")))
  :config
  (require 'omnisharp)
  (general-define-key :states 'normal
                      :keymaps 'csharp-mode-map
                      "gd" 'omnisharp-go-to-definition)
  (general-define-key :states '(motion normal visual)
                      :keymaps 'csharp-mode-map
                      ",u" 'omnisharp-find-usages
                      ",p" 'omnisharp-find-implementations
                      ",i" 'omnisharp-current-type-information
                      ",I" 'omnisharp-current-type-documentation
                      ",n" 'omnisharp-rename
                      ",r" 'omnisharp-run-code-action-refactoring)
  (general-define-key :states 'insert
                      :keymaps 'csharp-mode-map
                      "." 'omnisharp-add-dot-and-company-complete)
  (when (and IS-WINDOWS
             (executable-find "MSBuild.exe"))
    (defun modo-compile-csharp ()
      "Compiles the solution. Note that MSBuild.exe must be in your path."
      (interactive)
      (let ((sln-candidates (seq-filter (lambda (file) (f-ext-p file "sln"))
                                        (omnisharp--resolve-sln-candidates))))
        (cl-case (length sln-candidates)
          (0
           (error "No solution candidates found"))
          (1
           (compile (concat "MSBuild.exe " (car sln-candidates))))
          (t
           (compile (concat "MSBuild.exe " (completing-read "Select solution file:"
                                                            sln-candidates
                                                            nil
                                                            t)))))))
    (general-define-key :states '(motion normal visual)
                        :keymaps 'csharp-mode-map
                        ",c" '(modo-compile-csharp :wk "compile"))))

(straight-use-package 'omnisharp)
(use-package omnisharp
  :diminish omnisharp-mode
  :hook ((omnisharp-mode . modo-omnisharp-setup-hook))
  :init
  (setq omnisharp-autocomplete-want-documentation nil
        omnisharp-cache-directory (concat modo-cache-dir "omnisharp")))

(use-package nxml-mode
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.xaml\\'" . nxml-mode)
         ("\\.csproj\\'" . nxml-mode))
  :config
  (setq nxml-slash-auto-complete-flag t))

(provide 'modo-csharp)
;;; modo-csharp.el ends here

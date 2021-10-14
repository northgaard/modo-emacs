;;; modo-progutils.el --- Useful programming additions -*- lexical-binding: t -*-
;;; Commentary:

;; Support for programming adjacent stuff, such as markdown, json etc.

;;; Code:

;; Markdown
(straight-use-package 'markdown-mode)
(use-package markdown-mode)

;; JSON
(straight-use-package
 '(json-mode :type git
             :host github
             :repo "UwUnyaa/json-mode"))
(use-package json-mode)

;; XML
(use-package nxml-mode
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.xaml\\'" . nxml-mode)
         ("\\.csproj\\'" . nxml-mode))
  :config
  (setq nxml-slash-auto-complete-flag t))

;; YAML
(straight-use-package 'yaml-mode)
(use-package yaml-mode)

(use-package which-func ; built-in
  :init
  (modo-add-hook (prog-mode-hook
                  :name "modo--load-which-func"
                  :transient t)
    (require 'which-func))
  :custom
  (which-func-modes '(c-mode c++-mode python-mode ruby-mode))
  :config
  (setq which-func-unknown "")
  (which-function-mode 1))

(provide 'modo-progutils)
;;; modo-progutils.el ends here

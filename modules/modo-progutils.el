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

(provide 'modo-progutils)
;;; modo-progutils.el ends here

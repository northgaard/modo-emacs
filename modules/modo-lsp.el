;;; modo-lsp.el --- Language server support -*- lexical-binding: t -*-
;;; Commentary:

;; IDE like experience through the LSP protocol

;;; Code:

(straight-use-package 'lsp-mode)
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix (concat modo-leader " l"))
  :config
  (lsp-enable-which-key-integration t))

(provide 'modo-lsp)
;;; modo-lsp.el ends here

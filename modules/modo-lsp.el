;;; modo-lsp.el --- Language server support -*- lexical-binding: t -*-
;;; Commentary:

;; IDE like experience through the LSP protocol

;;; Code:

(straight-use-package 'lsp-mode)
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-session-file (concat modo-cache-dir "lsp-session-v1")
        lsp-server-install-dir (concat modo-cache-dir "lsp/")
        lsp-keymap-prefix "SPC l")
  :config
  (require 'company)
  (setq lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-which-key-integration t))

(provide 'modo-lsp)
;;; modo-lsp.el ends here

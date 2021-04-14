;;; modo-lsp.el --- Language server support -*- lexical-binding: t -*-
;;; Commentary:

;; IDE like experience through the LSP protocol

;;; Code:

(straight-use-package 'lsp-mode)
(use-package lsp-mode
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :hook (lsp-mode . evil-normalize-keymaps)
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-session-file (concat modo-cache-dir "lsp-session-v1")
        lsp-server-install-dir (concat modo-cache-dir "lsp/")
        lsp-keymap-prefix nil
        lsp-keep-workspace-alive nil)
  :config
  (modo-define-leader-key :keymaps 'lsp-mode-map
    "l" lsp-command-map)
  (require 'company)
  (setq lsp-headerline-breadcrumb-enable nil))

(provide 'modo-lsp)
;;; modo-lsp.el ends here

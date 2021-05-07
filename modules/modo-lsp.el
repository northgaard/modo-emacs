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
        lsp-keep-workspace-alive nil
        lsp-lens-enable t
        lsp-auto-guess-root t
        lsp-idle-delay 0.1)
  :config
  (modo-define-leader-key :keymaps 'lsp-mode-map
    "l" `(,lsp-command-map :wk "lsp"))
  (require 'flycheck)
  (require 'company)
  (setq lsp-headerline-breadcrumb-enable nil))

(defvar modo-consult-lsp-modes nil
  "List of major modes in which to use consult-lsp.")

(straight-use-package 'consult-lsp)
(use-package consult-lsp
  :after lsp-mode
  :demand t
  :config
  (-doto lsp-command-map
    (lsp-define-conditional-key
      "cd" consult-lsp-diagnostics "diagnostics" (memq major-mode modo-consult-lsp-modes)
      "cs" consult-lsp-symbols "symbols" (memq major-mode modo-consult-lsp-modes))))

(provide 'modo-lsp)
;;; modo-lsp.el ends here

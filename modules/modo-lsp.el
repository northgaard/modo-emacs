;;; modo-lsp.el --- Language server support -*- lexical-binding: t -*-
;;; Commentary:

;; IDE like experience through the LSP protocol

;;; Code:

(defvar-local modo-enable-lsp nil
  "Buffer local variable to determine whether the opened file
should use lsp-mode.")
(put 'modo-enable-lsp 'safe-local-variable #'booleanp)

(straight-use-package 'lsp-mode)
(use-package lsp-mode
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
  (require 'flymake)
  (require 'company)
  (setq lsp-headerline-breadcrumb-enable nil))

(straight-use-package '(consult-lsp :fork "northgaard"
                                    :branch "modo"))
(use-package consult-lsp
  :after lsp-mode
  :commands (consult-lsp-diagnostics consult-lsp-symbols consult-lsp-file-symbols)
  :config
  (evil-set-command-property 'consult-lsp-diagnostics :jump t)
  (evil-set-command-property 'consult-lsp-symbols :jump t)
  (evil-set-command-property 'consult-lsp-file-symbols :jump t))


(provide 'modo-lsp)
;;; modo-lsp.el ends here

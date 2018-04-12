;;; modo-flycheck.el --- flycheck base syntax checking -*- lexical-binding: t -*-
;;; Commentary:

;; Check syntax using flycheck.

;;; Code:
(straight-use-package 'flycheck)
(use-package flycheck
  :diminish flycheck-mode
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer))

(provide 'modo-flycheck)
;;; modo-flycheck.el ends here

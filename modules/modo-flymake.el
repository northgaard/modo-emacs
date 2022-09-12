;;; modo-flymake.el --- flymake base syntax checking -*- lexical-binding: t -*-
;;; Commentary:

;; Check syntax using flymake.

;;; Code:
(use-package flymake
  :defer 5
  :init
  (with-eval-after-load 'minions
    (push '(flymake-mode . nil) minions-available-modes)))

(provide 'modo-flymake)
;;; modo-flymake.el ends here

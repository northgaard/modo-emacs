;;; modo-flymake.el --- flymake base syntax checking -*- lexical-binding: t -*-
;;; Commentary:

;; Check syntax using flymake.

;;; Code:
(use-package flymake
  :defer 5
  :init
  (with-eval-after-load 'minions
    (push '(flymake-mode . nil) minions-available-modes))
  :config
  (evil-declare-not-repeat 'flymake-goto-next-error)
  (evil-declare-not-repeat 'flymake-goto-prev-error))

(provide 'modo-flymake)
;;; modo-flymake.el ends here

;;; modo-spellcheck.el --- the bulwark against pedants -*- lexical-binding: t -*-
;;; Commentary:

;; Check spelling using flyspell.

;;; Code:

(use-package flyspell
  :init
  (with-eval-after-load 'magit
    (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))
  :config
  (setq ispell-program-name (executable-find "hunspell")
        ispell-list-command "--list"
        ispell-extra-args '("--sug-mode=ultra")
        flyspell-issue-message-flag nil)
  (defun flyspell-prog-buffer ()
    (interactive)
    (let ((flyspell-generic-check-word-predicate
           #'flyspell-generic-progmode-verify))
      (flyspell-buffer)))
  (defun flyspell-clear-overlays ()
    (interactive)
    (flyspell-delete-all-overlays)))

(provide 'modo-spellcheck)
;;; modo-spellcheck.el ends here

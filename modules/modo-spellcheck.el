;;; modo-spellcheck.el --- the bulwark against pedants -*- lexical-binding: t -*-
;;; Commentary:

;; Check spelling using flyspell.

;;; Code:

(use-package flyspell
  :config
  (setq ispell-program-name (executable-find "aspell")
        ispell-list-command "--list")
  (defun flyspell-prog-buffer ()
    (interactive)
    (let ((flyspell-generic-check-word-predicate
           #'flyspell-generic-progmode-verify))
      (flyspell-buffer))))

(provide 'modo-spellcheck)
;;; modo-spellcheck.el ends here

;;; modo-spellcheck.el --- the bulwark against pedants -*- lexical-binding: t -*-
;;; Commentary:

;; Check spelling using flyspell.

;;; Code:

(defcustom modo-spellchecker-program
  (if (eq system-type 'windows-nt)
      "hunspell"
    "aspell")
  "Program used for spellchecking.")

(use-package flyspell
  :init
  (with-eval-after-load 'magit
    (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))
  :config
  (setq ispell-program-name (executable-find modo-spellchecker-program)
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

(straight-use-package 'flyspell-lazy)
(use-package flyspell-lazy
  :config
  (setq flyspell-lazy-idle-seconds 1.5
        flyspell-lazy-window-idle-seconds 15))

(define-minor-mode flyspell-lazy-local-mode
  "Turn on flyspell-lazy-local-mode.

This is more or less a local equivalent of the global
`flyspell-lazy-mode', but is local and automatically handles
turning on `flyspell-mode' as well. Using a local minor mode
allows for greater granularity in controlling spell checking. The
base `flyspell-mode' can make typing sluggish for larger buffers,
and so you would use this mode to prevent it. However, for some
buffers (for instance magit commit buffers), it's advantageous to
get the instant feedback of the base `flyspell-mode', and the
decrease in performance is worth it. Using the flyspell-lazy
package like this is not possible with the global
`flyspell-lazy-mode'."
  :group 'flyspell-lazy
  (progn
    (require 'flyspell-lazy)
    (cond
     (flyspell-lazy-local-mode
      (if (derived-mode-p 'prog-mode)
          (flyspell-prog-mode)
        (flyspell-mode 1))
      (flyspell-lazy-load))
     (t
      (flyspell-lazy-unload)
      (flyspell-mode -1)))))

(provide 'modo-spellcheck)
;;; modo-spellcheck.el ends here

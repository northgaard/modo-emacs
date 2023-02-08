;;; modo-elisp.el --- elisp editting -*- lexical-binding: t -*-
;;; Commentary:

;; Editing the engine itself!

;;; Code:

(defun modo--elisp-extra-fontification ()
  "Fontify modo functions."
  (font-lock-add-keywords
   nil `(("\\(^\\|\\s-\\|,\\)(\\(\\(modo\\|\\+\\)[^) ]+\\)[) \n]" (2 font-lock-keyword-face)))))

(add-hook 'emacs-lisp-mode-hook #'modo--elisp-extra-fontification)
(modo-add-hook (emacs-lisp-mode-hook :name "modo--set-company-backends-elisp")
  (setq-local company-backends '((company-capf company-dabbrev-code
                                               company-keywords
                                               company-files))))

(straight-use-package 'debbugs)
(use-package debbugs
  :config
  (evil-collection-require 'debbugs)
  (evil-collection-debbugs-setup)
  (evil-collection-define-key 'normal 'debbugs-gnu-mode-map
    (kbd modo-leader) nil)
  (setq debbugs-gnu-persistency-file (expand-file-name "debbugs" modo-cache-dir)))

(provide 'modo-elisp)
;;; modo-elisp.el ends here

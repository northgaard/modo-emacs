;;; flycheck-clazy.el --- Flycheck integration for clazy static checking -*- lexical-binding: t; -*-
;;; Commentary:

;; Flycheck integration for clazy, adapted from the code found here:
;; https://github.com/fuzzycode/flycheck-clazy/blob/master/flycheck-clazy.el

;;; Code:

(require 'flycheck)
(require 'seq)

(flycheck-def-config-file-var flycheck-clazy-compile-commands
    c++-clazy "compile_commands.json")
(flycheck-def-args-var flycheck-clazy-args c++-clazy)

(defun flycheck-clazy--find-project-root (_checker)
  (let ((project-root nil))
    (when (bound-and-true-p projectile-mode)
      (setq project-root (projectile-project-root)))
    (unless project-root
      (locate-dominating-file (buffer-file-name) "compile_commands.json"))
    (unless project-root
      (setq project-root (file-name-directory (buffer-file-name))))
    project-root))

(defun flycheck-clazy--verify (_checker)
  "Verifies CHECKER."
  (list (flycheck-verification-result-new
         :label "Project Root"
         :message (format "%s" (flycheck-clazy--find-project-root nil))
         :face (if (file-directory-p (flycheck-clazy--find-project-root nil)) 'success '(bold error)))))

(flycheck-define-checker c++-clazy
  "A flycheck backend for the clazy code analyzer."
  :command ("clazy-standalone"
            (config-file "-p" flycheck-clazy-compile-commands)
            (eval flycheck-clazy-args)
            "--ignore-included-files"
            source-original)
  :error-patterns ((error line-start (file-name) ":" line ":" column ": error: " (message) line-end)
                   (warning line-start (file-name) ":" line ":" column ": warning: " (message) line-end)
                   (info line-start (file-name) ":" line ":" column ": note: " (message) line-end))
  :working-directory flycheck-clazy--find-project-root
  :modes (c++-mode)
  :verify flycheck-clazy--verify
  :predicate (lambda () (and (buffer-file-name)
                             (flycheck-buffer-saved-p))))

(provide 'flycheck-clazy)

;;; flycheck-clazy.el ends here

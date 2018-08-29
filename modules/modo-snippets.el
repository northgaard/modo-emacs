;;; modo-snippets.el --- computer types so you don't have to -*- lexical-binding: t -*-
;;; Commentary:

;; Snippet expansion based on yasnippet.

;;; Code:
(straight-use-package 'yasnippet)
(use-package yasnippet
  :commands (yas-minor-mode
             yas-minor-mode-on)
  :hook ((text-mode prog-mode snippet-mode) . yas-minor-mode-on)
  :config
  (setq yas-snippet-dirs `(,(concat modo-emacs-dir "snippets/"))
        yas-triggers-in-field t
        yas-use-menu nil)
  (yas-reload-all))

(provide 'modo-snippets)
;;; modo-snippet.el ends here

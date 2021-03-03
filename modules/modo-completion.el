;;; modo-completion.el --- Minibuffer completion -*- lexical-binding: t -*-
;;; Commentary:

;; Minibuffer completion and associated goodies, currently with selectrum.

;;; Code:

(straight-use-package 'prescient)
(use-package prescient
  :demand t
  :config
  (setq prescient-save-file (concat modo-cache-dir "prescient-save.el")
        prescient-aggressive-file-save t)
  (prescient-persist-mode 1))

(straight-use-package 'selectrum)
(use-package selectrum
  :demand t
  :general
  (modo-define-leader-key "r" 'selectrum-repeat)
  (:keymaps 'selectrum-minibuffer-map
            "C-j" 'selectrum-next-candidate
            "C-k" 'selectrum-previous-candidate
            "C-w" 'backward-kill-word
            "C-d" 'selectrum-next-page
            "C-b" 'selectrum-previous-page
            "C-v" 'selectrum-goto-end
            "M-v" 'selectrum-goto-beginning
            "M-j" 'next-history-element
            "M-k" 'previous-history-element)
  :config
  (setq selectrum-count-style 'current/matches)
  (selectrum-mode 1))

(straight-use-package 'selectrum-prescient)
(use-package selectrum-prescient
  :demand t
  :config
  (selectrum-prescient-mode 1))

(straight-use-package 'marginalia)
(use-package marginalia
  :demand t
  :general
  (:keymaps 'selectrum-minibuffer-map
            "M-m" 'marginalia-cycle)
  :config
  (marginalia-mode 1)
  (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (advice-add #'marginalia-cycle :after
              (lambda () (selectrum-exhibit 'keep-selected))))


(provide 'modo-completion)
;;; modo-completion.el ends here

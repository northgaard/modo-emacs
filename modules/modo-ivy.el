;;; modo-ivy.el --- ivy-based completion -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration of ivy/counsel/swiper for modo emacs.

;;; Code:

(straight-use-package 'ivy)
(straight-use-package 'ivy-hydra)
(use-package ivy :demand t
  :commands (ivy-switch-buffer ivy-resume)
  :diminish ivy-mode
  :general
  (modo-define-leader-key "r" 'ivy-resume)
  (:keymaps 'ivy-minibuffer-map
            "C-j" 'ivy-next-line
            "C-k" 'ivy-previous-line
            "C-h" 'ivy-beginning-of-buffer
            "C-l" 'ivy-end-of-buffer
            "M-j" 'ivy-next-history-element
            "M-k" 'ivy-previous-history-element)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-wrap t
        smex-completion-method 'ivy
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil)
  (setq ivy-count-format "(%d/%d) ")
  (define-key ivy-mode-map [remap switch-to-buffer]
    #'ivy-switch-buffer)
  (use-package ivy-hydra
    :commands (hydra-ivy/body))
  (ivy-mode 1))

;; counsel-M-x uses smex when available
(straight-use-package 'smex)
(use-package smex
  :commands (smex smex-major-mode-commands)
  :config
  (setq smex-save-file (concat modo-cache-dir "smex-items"))
  (smex-initialize))

(straight-use-package 'counsel)
(use-package counsel
  :commands (counsel-apropos
             counsel-find-file
             counsel-recentf
             counsel-bookmark
             counsel-M-x
             counsel-describe-function
             counsel-describe-variable
             counsel-company)
  :init
  (define-key ivy-mode-map [remap apropos] #'counsel-apropos)
  (define-key ivy-mode-map [remap find-file] #'counsel-find-file)
  (define-key ivy-mode-map [remap recentf-open-files]
    #'counsel-recentf)
  (define-key ivy-mode-map [remap bookmark-jump] #'counsel-bookmark)
  (define-key ivy-mode-map [remap execute-extended-command]
    #'counsel-M-x)
  (define-key ivy-mode-map [remap describe-function] #'counsel-describe-function)
  (define-key ivy-mode-map [remap describe-variable] #'counsel-describe-variable))

(straight-use-package 'swiper)
(use-package swiper
  :commands (swiper)
  :init
  (define-key ivy-mode-map [remap isearch-forward] 'swiper))

(provide 'modo-ivy)
;;; modo-ivy.el ends here

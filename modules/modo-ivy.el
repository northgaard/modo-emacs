;;; modo-ivy.el --- ivy-based completion -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration of ivy/counsel/swiper for modo emacs.

;;; Code:

(straight-use-package 'ivy)
(use-package ivy
  :demand t
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
            "M-k" 'ivy-previous-history-element
            [remap switch-to-buffer] 'ivy-switch-buffer)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-wrap t
        smex-completion-method 'ivy
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil)
  (setq ivy-count-format "(%d/%d) "
        projectile-completion-system 'ivy)
  (ivy-mode 1))

(straight-use-package 'ivy-hydra)
(use-package ivy-hydra
  :demand t)

(straight-use-package 'ivy-rich)
(use-package ivy-rich
  :init
  ;; Load on calling rich ivy functions
  (defun modo--load-ivy-rich ()
    (require 'ivy-rich))
  (advice-add #'ivy-switch-buffer :before #'modo--load-ivy-rich)
  (advice-add #'counsel-M-x :before #'modo--load-ivy-rich)
  (advice-add #'counsel-recentf :before #'modo--load-ivy-rich)
  :config
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-rich-path-style 'abbrev)
  ;; Recommended by the ivy-rich README
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode 1)
  ;; Once loaded advice is no longer necessary
  (advice-remove #'ivy-switch-buffer #'modo--load-ivy-rich)
  (advice-remove #'counsel-M-x #'modo--load-ivy-rich)
  (advice-remove #'councel-recentf #'modo--load-ivy-rich))

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
  :general
  (:keymaps 'ivy-mode-map
            [remap apropos] 'counsel-apropos
            [remap find-file] 'counsel-find-file
            [remap recentf-open-files] 'counsel-recentf
            [remap execute-extended-command] 'counsel-M-x
            [remap describe-function] 'counsel-describe-function
            [remap describe-variable] 'counsel-describe-variable))

(straight-use-package 'counsel-projectile)
(use-package counsel-projectile
  :commands (counsel-projectile-find-file
             counsel-projectile-switch-to-buffer
             counsel-projectile-find-dir
             counsel-projectile-switch-project)
  :general
  (:keymaps 'ivy-mode-map
            [remap projectile-find-file] 'counsel-projectile-find-file
            [remap projectile-switch-to-buffer] 'projectile-switch-to-buffer
            [remap projectile-find-dir] 'counsel-projectile-find-dir
            [remap projectile-switch-project] 'counsel-projectile-switch-project)
  :config
  ;; Default to opening root project folder with dired
  (counsel-projectile-modify-action 'counsel-projectile-switch-project-action
                                    '((default 4))))

(straight-use-package 'swiper)
(use-package swiper
  :commands (swiper)
  :general
  (:keymaps 'ivy-mode-map
            [remap isearch-forward] 'swiper))

(provide 'modo-ivy)
;;; modo-ivy.el ends here

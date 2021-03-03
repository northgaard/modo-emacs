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
        smex-completion-method 'ivy)
  (setq ivy-count-format "(%d/%d) "
        ivy-read-action-function #'ivy-hydra-read-action)
  (setq ivy-switch-buffer-map (make-sparse-keymap))
  ;; The kill ring doesn't require sorting
  (push '(modo-kill-ring-insert) ivy-sort-functions-alist)
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
  :defer 3
  :commands (counsel-apropos
             counsel-find-file
             counsel-file-jump
             counsel-recentf
             counsel-bookmark
             counsel-M-x
             counsel-describe-function
             counsel-describe-variable
             counsel-company)
  :general
  (modo-define-leader-key
    :keymaps 'ivy-mode-map
    "fj" 'counsel-file-jump
    "fl" 'counsel-locate)
  (:keymaps 'ivy-mode-map
            [remap apropos] 'counsel-apropos
            [remap find-file] 'counsel-find-file
            [remap recentf-open-files] 'counsel-recentf
            [remap execute-extended-command] 'counsel-M-x
            [remap describe-function] 'counsel-describe-function
            [remap describe-variable] 'counsel-describe-variable)
  :config
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable))

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
  (:keymaps 'projectile-command-map
            "y" '(modo-switch-straight-project :wk "switch-straight-project"))
  :config
  (defun modo-switch-straight-project ()
    "Like `counsel-projectile-switch-project', but only lists
    projects cloned using straight.el"
    (interactive)
    (let ((projectile-known-projects (f-directories (straight--repos-dir)))
          (ivy--display-transformers-list '(counsel-projectile-switch-project
                                            abbreviate-file-name)))
      (call-interactively #'counsel-projectile-switch-project)))
  ;; Default to opening root project folder with dired
  (counsel-projectile-modify-action 'counsel-projectile-switch-project-action
                                    '((default 4))))

(straight-use-package 'swiper)
(use-package swiper
  :commands (swiper)
  :general
  (:keymaps 'ivy-mode-map
            [remap isearch-forward] 'swiper
            [remap isearch-backward] 'swiper-backward))

(provide 'modo-ivy)
;;; modo-ivy.el ends here

;;; modo-ui.el --- ui configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Core customization for pleasant viewing and interaction.

;;; Code:

;;; Base settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(blink-cursor-mode -1)
(setq-default indicate-empty-lines t)
(column-number-mode 1)
;; Window undo/redo
(winner-mode 1)
;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(when (eq system-type 'windows-nt)
  (add-hook 'emacs-startup-hook 'toggle-frame-maximized))
;; Highlight current line
(global-hl-line-mode 1)
;; Disable the bell
(setq ring-bell-function 'ignore)

;;; which-key
(straight-use-package 'which-key)
(use-package which-key :demand t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.7
        which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-compute-remaps t)
  ;; Show local bindings in bold
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-mode 1))

;;; window numbering
(straight-use-package 'window-numbering)
(use-package window-numbering :demand t
  :config
  (window-numbering-mode 1))

(straight-use-package 'avy)
(use-package avy :demand t
  :config
  (setq avy-timeout-seconds 0.5)
  (setq avy-background t))

(straight-use-package 'ace-link)
(use-package ace-link :demand t
  :config
  (ace-link-setup-default))

(straight-use-package 'ace-window)
(use-package ace-window
  :commands (ace-window ace-swap-window))

;;; Font
(when (display-graphic-p)
  (let ((preferred-font-families '("Hack"
                                   "Consolas"
                                   "Inconsolata"
                                   "DejaVu Sans Mono"))
        (existing-fonts '()))
    (dolist (font preferred-font-families)
      (when (modo-font-family-exists-p font)
        (add-to-list 'existing-fonts font t)))
    (add-to-list 'face-font-family-alternatives existing-fonts)
    (set-face-attribute 'default nil :family (car existing-fonts) :weight 'normal
                        :width 'normal :height 120)))

;;; Font scaling
(straight-use-package 'default-text-scale)
(use-package default-text-scale
  :init
  (defhydra hydra-font-size (:color red)
    "Change font size"
    ("+" default-text-scale-increase "increase")
    ("M-+" text-scale-increase "increase (buffer)")
    ("-" default-text-scale-decrease "decrease")
    ("M--" text-scale-decrease "decrease (buffer)")
    ("q" nil "quit")))

;;; Line numbers
(use-package linum :demand t
  :config
  (setq linum-format "%d "))

(straight-use-package 'hlinum)
(use-package hlinum :demand t
  :after linum
  :config
  (hlinum-activate))

(straight-use-package 'linum-relative)
(use-package linum-relative :demand t
  :after linum
  :config
  (setq linum-relative-format "%s ")
  (setq linum-relative-current-symbol "")
  (let ((toggle-linum (lambda ()
                        (linum-mode 1)
                        (linum-relative-on))))
    (add-hook 'text-mode-hook toggle-linum)
    (add-hook 'prog-mode-hook toggle-linum)))

;;; Sunburn theme
(straight-use-package 'sunburn-theme)
(use-package sunburn-theme
  :init
  (add-hook 'emacs-startup-hook
            (lambda () (load-theme 'sunburn t)))
  :config
  ;; Customizations for avy and ace-window
  (custom-set-faces
   '(avy-lead-face ((t (:background "#484349" :foreground "red" :inverse-video nil :weight bold))))
   '(avy-lead-face-0 ((t (:background "#484349" :foreground "midnight blue" :inverse-video nil :weight bold))))
   '(aw-background-face ((t (:background "#484349" :foreground "#aeadbd" :inverse-video nil))))
   '(aw-leading-char-face ((t (:background "#484349" :foreground "red"))))))

(provide 'modo-ui)
;;; modo-ui.el ends here

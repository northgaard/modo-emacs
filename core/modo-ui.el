;;; modo-ui.el --- ui configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Core customization for pleasant viewing and interaction.

;;; Code:

;;; Base settings
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
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

;;; which-key
(modo-add-package-single which-key "emacs-which-key/which-key.el")
(use-package which-key :demand t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.7)
  (setq which-key-sort-order #'which-key-prefix-then-key-order)
  (which-key-mode 1))

;;; window numbering
(modo-add-package-single window-numbering "window-numbering.el/window-numbering.el")
(use-package window-numbering :demand t
  :general
  (modo-define-leader-key "1" 'select-window-1
                          "2" 'select-window-2
                          "3" 'select-window-3
                          "4" 'select-window-4
                          "5" 'select-window-5
                          "6" 'select-window-6
                          "7" 'select-window-7
                          "8" 'select-window-8
                          "9" 'select-window-9)
  :config
  (window-numbering-mode 1))

;;; Font
(add-to-list 'default-frame-alist
             '(font . "Inconsolata-15"))

;;; Font scaling
(modo-add-package-single default-text-scale "default-text-scale/default-text-scale.el")
(use-package default-text-scale
  :init
  (defhydra hydra-font-size (:color red)
    "Change font size"
    ("+" default-text-scale-increase "increase")
    ("M-+" text-scale-increase "increase (buffer)")
    ("-" default-text-scale-decrease "decrease")
    ("M--" text-scale-decrease "decrease (buffer)")
    ("q" nil "quit"))
  :general
  (modo-define-leader-key "z" '(hydra-font-size/body :which-key "font-size")))

;;; Line numbers
(use-package linum :demand t
  :config
  (setq linum-format "%d "))

(modo-add-package-single hlinum "hlinum-mode/hlinum.el")
(use-package hlinum
  :after linum
  :config
  (hlinum-activate))

(modo-add-package-single linum-relative "linum-relative/linum-relative.el")
(use-package linum-relative
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
(modo-add-package-single sunburn-theme "Sunburn-Theme/sunburn-theme.el")
(use-package sunburn-theme
  :init
  (add-hook 'after-init-hook
            (lambda () (load-theme 'sunburn t)))
  :config
  ;; Customizations for avy
  (custom-set-faces
   '(avy-lead-face ((t (:background "#484349" :foreground "red" :inverse-video nil :weight bold))))
   '(avy-lead-face-0 ((t (:background "#484349" :foreground "midnight blue" :inverse-video nil :weight bold))))))

(provide 'modo-ui)
;;; modo-ui.el ends here

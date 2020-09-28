;;; modo-ui.el --- ui configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Core customization for pleasant viewing and interaction.

;;; Code:

;;; Base settings
(show-paren-mode 1)
(blink-cursor-mode -1)
(setq-default indicate-empty-lines t)
(column-number-mode 1)
;; Scrolling
(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      auto-window-vscroll nil)
;; Window undo/redo
(winner-mode 1)
;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(when IS-WINDOWS
  (add-hook 'emacs-startup-hook 'toggle-frame-maximized))
;; Highlight current line
(global-hl-line-mode 1)
;; Disable the bell
(setq ring-bell-function 'ignore)
;; Truncate lines by default
(setq-default truncate-lines t)

;;; which-key
(straight-use-package 'which-key)
(use-package which-key
  :demand t
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
(straight-use-package 'winum)
(use-package winum
  :demand t
  :init
  (setq winum-keymap (make-sparse-keymap))
  :config
  (setq winum-scope 'frame-local)
  (winum-mode))

(straight-use-package 'avy)
(use-package avy
  :demand t
  :config
  (setq avy-timeout-seconds 0.5)
  (setq avy-background t))

(straight-use-package 'ace-link)
(use-package ace-link
  :demand t
  :config
  (ace-link-setup-default))

(straight-use-package 'ace-window)
(use-package ace-window
  :commands (ace-window ace-swap-window))

;;; Font
(when (display-graphic-p)
  (let ((preferred-font-families '("Fira Code"
                                   "Hack"
                                   "Consolas"
                                   "Inconsolata"
                                   "DejaVu Sans Mono"))
        (existing-fonts '()))
    (dolist (font preferred-font-families)
      (when (modo-font-family-exists-p font)
        (cl-pushnew font existing-fonts)))
    (setq existing-fonts (nreverse existing-fonts))
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
(setq-default display-line-numbers-type 'relative
              display-line-numbers-current-absolute t
              display-line-numbers-width 2
              display-line-numbers-widen t)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;; Theme

(defface error-bold '((t (:inherit error :weight bold)))
  "Same as standard error face, but always bold."
  :group 'faces)

;; Doom themes
(straight-use-package 'doom-themes)
(defun modo--load-theme ()
  (load-theme 'doom-nord t)
  (doom-themes-set-faces 'user
    '(org-agenda-structure :foreground (doom-color 'fg) :weight 'ultra-bold :underline t)
    '(org-super-agenda-header :foreground (doom-color 'blue) :weight 'bold)))

(add-hook 'emacs-startup-hook #'modo--load-theme)

;;; Doom modeline
(straight-use-package 'doom-modeline)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'relative-to-project))

(provide 'modo-ui)
;;; modo-ui.el ends here

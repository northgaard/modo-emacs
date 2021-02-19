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

;; More intuitive behavior for new windows
;; From here: https://github.com/nex3/perspective-el#some-musings-on-emacs-window-layouts
(setq display-buffer-alist
      '((".*"
         (display-buffer-reuse-window display-buffer-same-window)
         (reusable-frames . t))))
(setq even-window-sizes nil)  ; display-buffer hint: avoid resizing

(use-package frame
  :demand t
  :config
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  (window-divider-mode 1))

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
(defcustom modo-allow-ligatures nil
  "If non-nil, allow experimental support for font ligatures."
  :type 'boolean
  :group 'modo-emacs)

(defvar modo-ligatures-active nil
  "Non-nil if ligatures are enabled and a font supporting them is used.")

(straight-use-package '(ligature :type git
                                 :host github
                                 :repo "mickeynp/ligature.el"))
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
                        :width 'normal :height 120)
    (when (and (string-equal (car existing-fonts) "Fira Code")
               modo-allow-ligatures)
      (setq modo-ligatures-active t)
      (ligature-set-ligatures 't '("www"))
      (ligature-set-ligatures 'prog-mode '("++" "--" "/=" "||" "||="
                                           "->" "=>" "::" "__"
                                           "==" "===" "!=" "=/=" "!=="
                                           "<=" ">=" "<=>"
                                           "/*" "*/" "//" "///"
                                           "<<" "<<<" "<<=" ">>" ">>>" ">>=" "|=" "^="))
      (global-ligature-mode 1))))

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

(defface modo-pulse-face '((t (:inherit hl-line)))
  "Face used for pulsing the current line."
  :group 'faces)

;; Doom themes
(straight-use-package 'doom-themes)
(defun modo--load-theme ()
  (load-theme 'doom-nord t)
  (doom-themes-set-faces 'user
    '(org-agenda-structure :foreground (doom-color 'fg) :weight 'ultra-bold :underline t)
    '(org-super-agenda-header :foreground (doom-color 'blue) :weight 'bold)
    '(modo-pulse-face :background (doom-lighten (face-attribute 'hl-line :background) 0.2))))

(add-hook 'emacs-startup-hook #'modo--load-theme)

;;; Doom modeline
(straight-use-package 'doom-modeline)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'relative-to-project))

;;; Pulse on window change, trying out to see if I like it
(defun modo-pulse-line (&rest _)
  (pulse-momentary-highlight-one-line (point) 'modo-pulse-face))

(define-minor-mode modo-pulse-line-mode
  "Turn on modo-pulse-line-mode.

This pulses the line at point when switching windows with winum
or ace-window. I've experienced flickering on some installations
with this, so it is not turned on by default."
  :group 'modo
  :global t
  (progn
    (require 'pulse)
    (let ((pulse-commands '(aw-switch-to-window
                            winum-select-window-1
                            winum-select-window-2
                            winum-select-window-3
                            winum-select-window-4
                            winum-select-window-5
                            winum-select-window-6
                            winum-select-window-7
                            winum-select-window-8
                            winum-select-window-9
                            winum-select-window-0-or-10)))
      (cond
       (modo-pulse-line-mode
        (dolist (command pulse-commands)
          (advice-add command :after #'modo-pulse-line)))
       (t
        (dolist (command pulse-commands)
          (advice-remove command #'modo-pulse-line)))))))

(provide 'modo-ui)
;;; modo-ui.el ends here

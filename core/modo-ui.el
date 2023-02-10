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
;; History lists
(setq history-delete-duplicates t)
;; Frame title
(setq-default frame-title-format '("%n %b - %F"))

;; More intuitive behavior for new windows
;; From here: https://github.com/nex3/perspective-el#some-musings-on-emacs-window-layouts
(setq display-buffer-alist
      '((".*"
         (display-buffer-reuse-window display-buffer-same-window)
         (reusable-frames . t))))
(setq even-window-sizes nil)  ; display-buffer hint: avoid resizing
(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

(use-package frame
  :demand t
  :config
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  (window-divider-mode 1))

(use-package fringe
  :config
  (set-fringe-mode (cdr (assoc "half-width" fringe-styles))))

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

;;; Window movement
(defvar modo-switch-window-hook nil
  "Hooks run after switching the focused window.")

(defun modo--switch-window-with-hooks (fn &rest args)
  (let ((window (selected-window)))
    (apply fn args)
    (unless (eq window (selected-window))
      (run-hooks 'modo-switch-window-hook))))

(straight-use-package 'winum)
(use-package winum
  :demand t
  :init
  (setq winum-keymap (make-sparse-keymap))
  :config
  (advice-add 'winum-select-window-1 :around #'modo--switch-window-with-hooks)
  (advice-add 'winum-select-window-2 :around #'modo--switch-window-with-hooks)
  (advice-add 'winum-select-window-3 :around #'modo--switch-window-with-hooks)
  (advice-add 'winum-select-window-4 :around #'modo--switch-window-with-hooks)
  (advice-add 'winum-select-window-5 :around #'modo--switch-window-with-hooks)
  (advice-add 'winum-select-window-6 :around #'modo--switch-window-with-hooks)
  (advice-add 'winum-select-window-7 :around #'modo--switch-window-with-hooks)
  (advice-add 'winum-select-window-8 :around #'modo--switch-window-with-hooks)
  (advice-add 'winum-select-window-9 :around #'modo--switch-window-with-hooks)
  (advice-add 'winum-select-window-0-or-10 :around #'modo--switch-window-with-hooks)
  (setq winum-scope 'frame-local)
  (defun winum-delete-ignored-windows ()
    "Deletes all currently open windows that are not selectable
by winum."
    (interactive)
    (dolist (win (window-list))
      (when (winum--ignore-window-p win)
          (delete-window win))))
  (winum-mode))

(use-package windmove
  :demand t
  :config
  (advice-add 'windmove-down :around #'modo--switch-window-with-hooks)
  (advice-add 'windmove-up :around #'modo--switch-window-with-hooks)
  (advice-add 'windmove-left :around #'modo--switch-window-with-hooks)
  (advice-add 'windmove-right :around #'modo--switch-window-with-hooks))

;;; Keyboard driven selection
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

;;; Window size adjustment
(straight-use-package '(move-border :type git :host github :repo "ramnes/move-border"))
(use-package move-border
  :init
  (defhydra hydra-window-resize (:body-pre (progn
                                             (modo-temporary-set
                                              which-key-inhibit t
                                              hydra-hint-display-type 'posframe)
                                             (require 'move-border))
                                           :post (modo-temporary-reset))
    ;;newline is necessary here!
    "
^Resize Windows^ "
  ;;Entry
  ("H" (lambda () (interactive) (move-border-left 5)) "left" :column "Large")
  ("J" (lambda () (interactive) (move-border-down 5)) "down")
  ("K" (lambda () (interactive) (move-border-up 5)) "up")
  ("L" (lambda () (interactive) (move-border-right 5)) "right")
  ("=" (lambda () (interactive) (balance-windows)) "balance-windows" :column "Balance")
  ("h" (lambda () (interactive) (move-border-left 1)) "left" :column "Small")
  ("j" (lambda () (interactive) (move-border-down 1)) "down")
  ("k" (lambda () (interactive) (move-border-up 1)) "up")
  ("l" (lambda () (interactive) (move-border-right 1)) "right")
  ("q" nil "quit" :column "End")))

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
  (defhydra hydra-font-size (:color red
                                    :body-pre (modo-temporary-set
                                               hydra-hint-display-type
                                               'message)
                                    :post (modo-temporary-reset))
    "Change font size"
    ("+" default-text-scale-increase "increase")
    ("M-+" text-scale-increase "increase (buffer)")
    ("-" default-text-scale-decrease "decrease")
 ("M--" text-scale-decrease "decrease (buffer)")
    ("q" nil "quit")))

;;; Line numbers
(setq-default display-line-numbers-type 'relative
              display-line-numbers-current-absolute t
              display-line-numbers-width-start t
              display-line-numbers-widen t)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;; Highlight todo type notes
(straight-use-package 'hl-todo)
(use-package hl-todo
  :init
  (modo-add-hook ((text-mode-hook prog-mode-hook)
                  :name "modo--hl-todo-hook"
                  :transient t)
    (global-hl-todo-mode 1))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces '(("TODO" warning bold)
                                ("FIXME" error bold)
                                ("HACK" font-lock-constant-face bold)
                                ("REVIEW" font-lock-keyword-face bold)
                                ("NOTE" success bold)
                                ("DEPRECATED" font-lock-doc-face bold)
                                ("BUG" error bold)
                                ("XXX" font-lock-constant-face bold))))

;;; Theme
(when (window-system)
  (defun modo--load-theme ()
    (load-theme 'modus-vivendi t))
  (add-hook 'emacs-startup-hook #'modo--load-theme))

;;; Doom modeline
(straight-use-package 'all-the-icons)
(straight-use-package 'doom-modeline)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-minor-modes t))

(straight-use-package 'minions)
(use-package minions
  :demand t
  :after doom-modeline
  :config
  (minions-mode 1))

;;; Tabs
(use-package tab-bar
  :custom
  (tab-bar-show 1))

(provide 'modo-ui)
;;; modo-ui.el ends here

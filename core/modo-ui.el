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
(straight-use-package 'window-numbering)
(use-package window-numbering
  :demand t
  :config
  (window-numbering-mode 1))

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
(use-package linum
  :demand t
  :config
  (setq linum-format "%d "))

(straight-use-package 'hlinum)
(use-package hlinum
  :demand t
  :after linum
  :config
  (hlinum-activate))

(straight-use-package 'linum-relative)
(use-package linum-relative
  :demand t
  :after linum
  :hook ((text-mode prog-mode) . (lambda ()
                                   (linum-mode 1)
                                   (linum-relative-on)))
  :config
  (setq linum-relative-format "%3s ")
  (setq linum-relative-current-symbol ""))

;;; Themes
;; Sunburn
(straight-use-package 'sunburn-theme)
(use-package sunburn-theme
  :config
  ;; Customizations for avy and ace-window
  (defun modo--sunburn-customizations ()
    (sunburn-with-color-variables
      (custom-theme-set-faces
       'sunburn
       `(avy-lead-face ((t (:background ,sunburn-bg :foreground "red" :inverse-video nil :weight bold))))
       `(avy-lead-face-0 ((t (:background ,sunburn-bg :foreground "red" :inverse-video nil :weight bold))))
       `(avy-lead-face-1 ((t (:background ,sunburn-bg :foreground "red" :inverse-video nil :weight bold))))
       `(avy-lead-face-2 ((t (:background ,sunburn-bg :foreground "red" :inverse-video nil :weight bold))))
       `(aw-background-face ((t (:background ,sunburn-bg :foreground ,sunburn-fg-1 :inverse-video nil))))
       `(aw-leading-char-face ((t (:background ,sunburn-bg :foreground "red"))))))))

;; Kaolin
(straight-use-package '(kaolin-themes :host github
                                      :repo "ogdenwebb/emacs-kaolin-themes"
                                      :files (:defaults "themes/*.el")
                                      :fork (:repo "semutir/emacs-kaolin-themes"
                                                   :branch "modo")))

(defun modo-load-theme (theme)
  "Loads the theme THEME and optionally calls a function providing custom
modifications. This function should be named modo--THEME-customizations and
take no arguments."
  (interactive
   (list
    (intern (completing-read "Choose theme to load: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (let* ((theme-name (symbol-name theme))
         (theme-customization-fun (intern-soft (format "modo--%s-customizations" theme-name))))
    (unless (custom-theme-name-valid-p theme)
      (error "Invalid theme name `%s'" theme))
    (load-theme theme t)
    (when (functionp theme-customization-fun)
      (funcall theme-customization-fun))))

;; TODO: Add hydra for changing themes
(add-hook 'emacs-startup-hook (lambda () (modo-load-theme 'kaolin-dark)))

;;; Doom modeline
(straight-use-package 'doom-modeline)
(use-package doom-modeline
  :hook (after-init . doom-modeline-init)
  :config
  (setq doom-modeline-buffer-file-name-style 'relative-to-project))

(provide 'modo-ui)
;;; modo-ui.el ends here

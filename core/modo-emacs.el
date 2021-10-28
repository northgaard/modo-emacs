;;; modo-emacs.el --- emacs configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Core customization and extension of built-in emacs functionality.

;;; Code:

;;; emacs server
(use-package server
  :demand t
  :init
  (setq server-auth-dir (concat modo-cache-dir "server/"))
  :config
  (unless (eq (server-running-p) t)
    (server-start)))

;;; eshell
(use-package eshell
  :init
  (setq eshell-directory-name (concat modo-cache-dir "eshell/")))

;;; Compilation
(use-package compile
  :config
  (setq compilation-scroll-output 'first-error)
  (when (featurep 'dbusbind)
    (require 'notifications)
    (defun modo-on-compilation-finished (buffer description)
      (let ((title (buffer-name buffer)))
        (unless (string-match-p "grep" title)
          (notifications-notify
           :title title
           :body description))))
    (add-to-list 'compilation-finish-functions #'modo-on-compilation-finished)))

;;; ansi-color
(use-package ansi-color
  :preface
  (autoload 'ansi-color-apply-on-region "ansi-color")
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  :hook (compilation-filter . colorize-compilation-buffer)
  :config
  (add-hook 'comint-preoutput-filter-functions 'ansi-color-filter-apply))

;;; ag from emacs
(straight-use-package 'ag)

;;; Powershell
(straight-use-package 'powershell)

;;; Helpful -- better help buffers
(straight-use-package 'helpful)
(use-package helpful
  :init
  (modo-define-leader-key :keymaps 'override
    "hl" 'view-lossage)
  :general
  (modo-define-leader-key :keymaps 'override
    "h"  '(:ignore t :wk "helpful")
    "hk" 'helpful-key
    "hf" 'helpful-function
    "hv" 'helpful-variable
    "hs" 'helpful-symbol
    "hh" 'helpful-at-point
    "hc" 'helpful-callable
    "hx" 'helpful-command
    "hm" 'helpful-macro)
  :config
  (evil-collection-require 'helpful)
  (evil-collection-helpful-setup)
  (evil-collection-define-key 'normal 'helpful-mode-map
    "o" 'ace-link
    "q" 'quit-window))

(provide 'modo-emacs)
;;; modo-emacs.el ends here

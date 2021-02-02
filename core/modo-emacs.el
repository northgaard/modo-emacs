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
  (setq compilation-scroll-output 'first-error))

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

(provide 'modo-emacs)
;;; modo-emacs.el ends here

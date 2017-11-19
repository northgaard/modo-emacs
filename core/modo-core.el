;;; modo-core.el --- core settings -*- lexical-binding: t -*-
;;; Commentary:

;; Core settings and definitions that must always be loaded.

;;; Code:

;;; Directory variables
(defvar modo-emacs-dir (file-truename (expand-file-name user-emacs-directory))
  "The path to the emacs.d directory containing the modo emacs files.")

(defvar modo-core-dir (concat modo-emacs-dir "core/")
  "The directory containing the core of modo emacs.")

(defvar modo-repo-dir (concat modo-emacs-dir "repos/")
  "The directory containing the modo repositories.")
(unless (file-exists-p modo-repo-dir)
  (make-directory modo-repo-dir))

(defvar modo-cache-dir (concat modo-emacs-dir "cache/")
  "The directory storing persistent information.")
(unless (file-exists-p modo-cache-dir)
  (make-directory modo-cache-dir))

(defvar modo-temp-dir (concat modo-emacs-dir "temp/")
  "The directory storing temporary files.")
(unless (file-exists-p modo-temp-dir)
  (make-directory modo-temp-dir))

(defvar modo-build-dir (concat modo-emacs-dir "build/")
  "The directory where packages are built.")

;;; UTF-8 all the things
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;;; Custom file
(setq custom-file (expand-file-name "custom.el" modo-emacs-dir))
(load custom-file t t)

;;; Initial high threshold for garbage collection
(let ((normal-gc-cons-threshold (* 20 1024 1024)) ;; ~20 mb
      (init-gc-cons-threshold (* 128 1024 1024))) ;; ~128 mb
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;; Load package system
(require 'modo-package (concat modo-core-dir "modo-package"))

;;; Load rest of core
(unless noninteractive
  ;; Keybind definers
  ;; hydra
  (modo-add-package hydra "hydra")
  (use-package hydra :demand t)

  ;; general.el --- get your keybinds here!
  (modo-add-package-single general "general.el/general.el")
  (use-package general :demand t)

  ;; Leader keys
  (defcustom modo-leader "SPC"
    "The general purpose leader accessible from normal mode.")

  (defcustom modo-non-normal-leader "C-c"
    "Equivalent to the normal mode leader, but used in insert and emacs mode.")

  (defcustom modo-major-leader ","
    "Shortcut for major mode keys, also bound to \"<leader> m\"")

  ;; Definer for standard shortcuts
  (general-create-definer modo-define-leader-key
                          :states '(motion normal visual insert emacs)
                          :prefix modo-leader
                          :non-normal-prefix modo-non-normal-leader
                          :prefix-command 'modo-leader-command)
  (general-create-definer modo-define-major-leader-key
                          :states '(motion normal visual)
                          :prefix modo-major-leader
                          :prefix-command 'modo-major-leader-command)
  ;; Rest of the core features
  (require 'modo-editor)
  (require 'modo-ui)
  (require 'modo-utils)
  (require 'modo-keybinds))

(provide 'modo-core)
;;; modo-core.el ends here

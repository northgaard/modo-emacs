;;; init.el -*- lexical-binding: t -*-

;;; Check version
(defconst modo-minimum-emacs-version "28.1"
  "The minimum version of emacs required by modo emacs.")

(when (version< emacs-version modo-minimum-emacs-version)
  (error (format "Error: modo emacs requires emacs version %s, you have %s"
                 modo-minimum-emacs-version emacs-version)))

;;; Explicitly load early-init.el if it hasn't been (i.e. if this file
;;; is loaded as part of a batch run).
(unless (boundp 'modo--early-init-loaded)
  (load (concat (file-name-directory load-file-name) "early-init")
        nil t))

(require 'modo-core (concat user-emacs-directory "core/modo-core"))
(modo-module completion
             git
             progutils
             gdb
             company
             org
             dired
             latex
             snippets
             spellcheck
             flymake
             lsp
             elisp
             cpp
             rust
             groovy)

;;; Finally, load the private init file, if it exists
(load (concat modo-private-dir "init.el") t t)

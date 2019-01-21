;;; init.el -*- lexical-binding: t -*-

;;; Check version
(defconst modo-minimum-emacs-version "26.1"
  "The minimum version of emacs required by modo emacs.")

(when (version< emacs-version modo-minimum-emacs-version)
  (error (format "Error: modo emacs requires emacs version %s, you have %s"
                 modo-minimum-emacs-version emacs-version)))

(require 'modo-core (concat user-emacs-directory "core/modo-core"))
(modo-module ivy
             git
             company
             org
             latex
             snippets
             spellcheck
             flycheck
             csharp)

;;; elisp
(defun modo--elisp-extra-fontification ()
  "Fontify modo functions."
  (font-lock-add-keywords
   nil `(("\\(^\\|\\s-\\|,\\)(\\(\\(modo\\|\\+\\)[^) ]+\\)[) \n]" (2 font-lock-keyword-face)))))

(add-hook 'emacs-lisp-mode-hook #'modo--elisp-extra-fontification)

;;; Finally, load the private init file, if it exists
(load modo-private-init-file t t)

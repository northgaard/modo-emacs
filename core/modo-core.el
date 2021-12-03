;;; modo-core.el --- core settings -*- lexical-binding: t -*-
;;; Commentary:

;; Core settings and definitions that must always be loaded.

;;; Code:

(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-WSL     (and IS-LINUX (getenv "WSLENV"))) ; Not bullet-proof, WSLENV is not implicitly defined
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

(defgroup modo-emacs nil
  "A modular and modal emacs configuration."
  :group 'applications
  :prefix "modo-")

;;; Directory variables
(defvar modo-emacs-dir (file-truename (expand-file-name user-emacs-directory))
  "The path to the emacs.d directory containing the modo emacs files.")

(defvar modo-core-dir (concat modo-emacs-dir "core/")
  "The directory containing the core of modo emacs.")

(defvar modo-modules-dir (concat modo-emacs-dir "modules/")
  "The directory containing modo modules.")

(defvar modo-cache-dir (concat modo-emacs-dir "cache/")
  "The directory storing persistent information.")
(unless (file-exists-p modo-cache-dir)
  (make-directory modo-cache-dir))

(defvar modo-temp-dir (concat modo-emacs-dir "temp/")
  "The directory storing temporary files.")
(unless (file-exists-p modo-temp-dir)
  (make-directory modo-temp-dir))

(defvar modo-private-dir (concat modo-emacs-dir "private/")
  "Directory for storing private configuration files.")
(unless (file-exists-p modo-private-dir)
  (make-directory modo-private-dir))

(defconst modo-system-identifier (concat (user-login-name) "@" (system-name))
  "Identifier for the system running modo emacs.")

;; Native comp
(setq native-comp-async-report-warnings-errors nil)

;; Fallback for HOME environment variable on Windows
(when (and IS-WINDOWS (null (getenv-internal "HOME")))
  (setenv "HOME" (getenv "USERPROFILE"))
  (setq abbreviated-home-dir nil))

;;; UTF-8
;; I have sort of given up on figuring out what is the "proper" way to
;; handle this. This is what Doom Emacs currently has, which I suppose
;; is good enough for me too.
(set-language-environment "UTF-8")
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8))

;;; Unified setting of variables
(defmacro modo-set-1 (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))
(defmacro modo-set (&rest binds)
  (pcase binds
    (`(,var ,val)
     `(modo-set-1 ,var ,val))
    (`(,var ,val . ,rest)
     `(progn
        (modo-set-1 ,var ,val)
        (modo-set ,@rest)))))

(defvar modo--temporary-restore nil)
(defmacro modo-temporary-set-1 (variable value)
  (let ((old-value (symbol-value variable)))
    `(progn
       (cl-pushnew '(,variable . ,old-value) modo--temporary-restore
                   :test 'equal)
       (modo-set-1 ,variable ,value))))
(defmacro modo-temporary-set (&rest binds)
  (pcase binds
    (`(,var ,val)
     `(modo-temporary-set-1 ,var ,val))
    (`(,var ,val . ,rest)
     `(progn
        (modo-temporary-set-1 ,var ,val)
        (modo-temporary-set ,@rest)))))
(defun modo-temporary-reset ()
  (dolist (restore modo--temporary-restore)
    (eval (if (symbolp (cdr restore))
              `(modo-set-1 ,(car restore) ',(cdr restore))
            `(modo-set-1 ,(car restore) ,(cdr restore)))))
  (setq modo--temporary-restore nil))

;;; Custom file
(setq custom-file (expand-file-name "custom.el" modo-emacs-dir))
(load custom-file t t)

;;; Temp directory
(setq temporary-file-directory modo-temp-dir)

;; Be quiet at startup
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

;;; Set empty file-name-handler-alist to slightly boost startup time
(let ((base-file-name-handler-alist file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist
                    (append base-file-name-handler-alist file-name-handler-alist))
              (cl-delete-duplicates file-name-handler-alist :test 'equal))))

;;; Settings pertaining to Emacs itself that do not fit better elsewhere
(setq inhibit-compacting-font-caches t
      enable-recursive-minibuffers t
      read-process-output-max (* 1024 1024))
;; Some hacks regarding directory local variables, see first answer here:
;; https://stackoverflow.com/questions/5147060/how-can-i-access-directory-local-variables-in-my-major-mode-hooks
;; TODO probably could organize this more elegantly
(defun modo-run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (concat (symbol-name major-mode) "-local-vars-hook"))))
(add-hook 'hack-local-variables-hook 'modo-run-local-vars-mode-hook)

;; Add core dir and modules dir to load path
(add-to-list 'load-path modo-core-dir)
(add-to-list 'load-path modo-modules-dir)

;;; Just get it over with (built in)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'thunk)

;; Load operating system specific configuration as early as possible
(require 'modo-os)
;;; Load package system
(require 'modo-package)

(straight-use-package 'gcmh)
(use-package gcmh
  :diminish gcmh-mode
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 500 1024 1024)) ; 500 mb, this actually 1/2 the default!
  (add-hook 'emacs-startup-hook
            (lambda () (gcmh-mode 1))))

;;; Just get it over with (external)
(straight-use-package 'dash)
(straight-use-package 's)
(straight-use-package 'f)

(require 'dash)
(dash-enable-font-lock)
(require 's)
(require 'f)

;;; Load shared library functions
(require 'modo-lib)

;;; Load rest of core

;; Keybind definers
;; hydra
(straight-use-package 'lv)
(straight-use-package 'posframe)
(straight-use-package 'hydra)
(use-package hydra
  :demand t)
(straight-use-package '(transient :type built-in))

;; general.el --- get your keybinds here!
(straight-use-package 'general)
(use-package general
  :demand t
  :config
  (general-override-mode))

;; Leader keys
(defconst modo-leader "SPC"
  "The general purpose leader accessible from normal mode.")

(defconst modo-non-normal-leader "C-c"
  "Equivalent to the normal mode leader, but used in insert and emacs mode.")

(defconst modo-major-leader ","
  "Shortcut for major mode keys, also bound to \"<leader> m\"")

;; Definer for standard shortcuts
(general-create-definer modo-define-leader-key
  :states '(motion normal visual insert emacs)
  :prefix modo-leader
  :non-normal-prefix modo-non-normal-leader)
(general-create-definer modo--direct-major-leader-key
  :states '(motion normal visual)
  :prefix modo-major-leader
  "" '(:ignore t
       :which-key (lambda (arg)
                      `(,(cadr (split-string (car arg) " ")) .
                          ,(replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))
(general-create-definer modo--indirect-major-leader-key
  :states '(motion normal visual insert emacs)
  :prefix (concat modo-leader " m")
  :non-normal-prefix (concat modo-non-normal-leader " m")
  "" '(:ignore t
       :which-key (lambda (arg)
                      `(,(cadr (split-string (car arg) " ")) .
                          ,(replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))
(defmacro modo-define-major-leader-key (&rest args)
  "Defines leader key bindings for a major mode. Commands are
bound both under <major-leader>, as well as \"<leader> m\"."
  (declare (indent defun))
  (let ((expansion nil))
    (push `(modo--direct-major-leader-key ,@args) expansion)
    (push `(modo--indirect-major-leader-key ,@args) expansion)
    `(progn
       ,@expansion)))

;; Rest of the core features
(require 'modo-evil)
(require 'modo-utils)
(require 'modo-editor)
(require 'modo-emacs)
(require 'modo-projects)
(require 'modo-ui)
(require 'modo-keybinds)

(provide 'modo-core)
;;; modo-core.el ends here

;;; modo-gdb.el --- gdb integration -*- lexical-binding: t -*-
;;; Commentary:

;; Debug stuff with GDB from within Emacs.

;;; Code:

(defcustom modo-gdb-favorites nil
  "Alist mapping a name to a command line program to be run under gdb.")

(defvar modo--gdb-session-number 0)

(defun modo--close-tab-if-gdb ()
  (let ((tab-name (modo-current-tab-name)))
    (when (string-match-p "gdb-session-[0-9]+" tab-name)
      (tab-bar-close-tab-by-name tab-name))))

(use-package gud
  :general
  (modo-define-leader-key
    :keymaps 'override
    "g" '(:ignore t :wk "debug")
    "gb" 'gud-break
    "gd" 'gud-remove
    "gu" 'gud-until
    "gR" 'gud-run
    "gc" 'gud-cont
    "gf" 'gud-finish
    "gt" 'gud-tbreak
    "gn" 'gud-next
    "gs" 'gud-step))

(use-package gdb-mi
  :general
  (modo-define-leader-key
    :keymaps 'override
    "gF" 'modo-gdb-select-favorite
    "gG" 'gdb
    "gC" 'gdb-io-interrupt
    "gQ" 'gdb-delchar-or-quit
    "gw" 'gdb-restore-windows
    "gW" 'gdb-many-windows)
  :config
  (setq gdb-many-windows t
        gdb-show-main t
        gdb-display-io-nopopup t)
  (advice-add #'gdb-reset :after #'modo--close-tab-if-gdb)
  (defun modo-gdb-select-favorite (name)
    "Select NAME from `modo-gdb-favorites' and debug the
corresponding program. By default, the gdb debugger window
configuration is launched in a new tab. With a prefix argument,
this is not the case."
    (interactive
     (list (completing-read "To debug: " (mapcar #'car modo-gdb-favorites))))
    (if-let ((program-arg (alist-get name modo-gdb-favorites nil nil #'string=)))
        (progn
          (unless (equal current-prefix-arg '(4))
            (tab-bar-new-tab)
            (setq modo--gdb-session-number (1+ modo--gdb-session-number))
            (tab-bar-rename-tab (format "gdb-session-%d" modo--gdb-session-number)))
          (gdb (concat "gdb -i=mi --args " program-arg)))
      (user-error "Not a valid entry!")))
  (evil-collection-require 'gdb)
  (evil-collection-gdb-setup))

(provide 'modo-gdb)
;;; modo-gdb.el ends here

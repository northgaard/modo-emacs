;;; modo-gdb.el --- gdb integration -*- lexical-binding: t -*-
;;; Commentary:

;; Debug stuff with GDB from within Emacs.

;;; Code:

(defcustom modo-gdb-favorites nil
  "Alist mapping a name to a command line program to be run under gdb.")

(use-package gdb-mi
  :general
  (modo-define-leader-key
    :keymaps 'override
    "g" '(:ignore t :wk "debug")
    "gF" 'modo-gdb-select-favorite)
  :config
  (setq gdb-many-windows t
        gdb-show-main t)
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
            (tab-bar-new-tab))
          (gdb (concat "gdb -i=mi --args " program-arg)))
      (user-error "Not a valid entry!"))))

(provide 'modo-gdb)
;;; modo-gdb.el ends here

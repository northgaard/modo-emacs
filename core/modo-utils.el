;;; modo-utils.el --- utility functions -*- lexical-binding: t -*-
;;; Commentary:

;; Useful interactive functions.

;;; Code:

(defun modo-find-dotfile ()
  "Opens init.el in modo-emacs-dir."
  (interactive)
  (let* ((dotfile (file-truename (expand-file-name "init.el" modo-emacs-dir)))
         (buffer-name (get-file-buffer dotfile)))
    (if buffer-name
        (switch-to-buffer buffer-name) ;; If buffer already exists, simply switch to it
      (find-file dotfile)
      ;; Needed to make saveplace work with this function
      (run-hooks 'find-file-hook))))

(defun modo--extract-name (string)
  (let* ((str (file-name-sans-extension string))
         (split (split-string str "-")))
    (string-join (cdr split) "-")))

(defun modo--get-features (dir)
  (let ((features nil)
        (files (directory-files dir nil "modo")))
    (dolist (file files)
      (push (modo--extract-name file) features))
    (nreverse features)))

(defun modo-find-core-file (name)
  "Opens the core file modo-NAME.el in modo-core-dir."
  (interactive
   (list (completing-read "Core file: " (modo--get-features modo-core-dir))))
  (let* ((core-file (file-truename
                     (expand-file-name (format "modo-%s.el" name)
                                       modo-core-dir)))
         (buffer-name (get-file-buffer core-file)))
    (if buffer-name
        (switch-to-buffer buffer-name)
      (if (file-exists-p core-file)
          (progn
            (find-file core-file)
            (run-hooks 'find-file-hook))
        (error "'%s' is not a core file!" core-file)))))

(defun modo-find-module-file (name)
  "Opens the module file modo-NAME.el in modo-module-dir."
  (interactive
   (list (completing-read "Module file: " (modo--get-features modo-modules-dir))))
  (let* ((module-file (file-truename
                       (expand-file-name (format "modo-%s.el" name)
                                         modo-modules-dir)))
         (buffer-name (get-file-buffer module-file)))
    (if buffer-name
        (switch-to-buffer buffer-name)
      (if (file-exists-p module-file)
          (progn
            (find-file module-file)
            (run-hooks 'find-file-hook))
        (error "'%s' is not a module file!" module-file)))))

(defun modo-delete-auto-save-file ()
  "Delete the autosave file in the currently visited buffer, if it exists."
  (interactive)
  (let ((auto-file (file-truename (make-auto-save-file-name))))
    (if (and (buffer-file-name)
             (file-exists-p auto-file))
        (progn
          (delete-file auto-file)
          (message (format "Deleted file %s." (file-name-nondirectory auto-file))))
      (message "No auto-save file exists."))))

;; Two useful functions borrowed from Steve Purcell
(defun modo-delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun modo-rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun modo-alternate-buffer ()
  "Switch between current and last buffer."
  (interactive)
  (if (fboundp 'evil-alternate-buffer)
      (switch-to-buffer (car (evil-alternate-buffer)))
    (switch-to-buffer (other-buffer (current-buffer) t))))

(defun modo-font-family-exists-p (family-name)
  "Checks if the font family FAMILY-NAME exists. Returns the font-entity
if it does, nil otherwise."
  (find-font (font-spec :family family-name)))

(defun modo--get-subdirs (path)
  (mapcar #'file-name-nondirectory (f-directories path)))

(provide 'modo-utils)
;;; modo-utils.el ends here

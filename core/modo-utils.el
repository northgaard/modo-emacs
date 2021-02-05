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
  (interactive (list (read-string "New name: "
                                  (file-name-nondirectory (buffer-file-name)))))
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
  (let ((buf (window-buffer)))
    (switch-to-buffer
     (cl-find-if (lambda (b) (not (eq b buf)))
                 (mapcar 'car (window-prev-buffers))))))

(defun modo-get-faces (pos)
  ;; TODO: Add documentation
  ""
  (interactive "d")
  (let ((faces (-flatten
                (remq nil
                      (list
                       (get-char-property pos 'read-face-name)
                       (get-char-property pos 'face)
                       (plist-get (text-properties-at pos) 'face))))))
    (if (called-interactively-p 'interactive)
        (message (format "%s" faces))
      faces)))

(defun modo-kill-non-default-buffers ()
  "Kill all buffers except the startup ones."
  (interactive)
  ;; I don't currently have a dashboard, but historically I change my mind...
  (let ((preserved-buffers '("*dashboard*" "*Messages*" "*scratch*")))
    (mapc (lambda (buffer)
            (unless (member (buffer-name buffer) preserved-buffers)
              (kill-buffer buffer)))
          (buffer-list))))

(defun modo-toggle-window-split ()
  "Toggle between horizontal and vertial split, when there are
two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; Adapted from:
;; https://www.emacswiki.org/emacs/BrowseKillRing
(defun modo-kill-ring-insert (to-insert)
  (interactive
   (let ((completion-list (cl-delete-duplicates kill-ring :test #'equal)))
     (list (completing-read "Yank: " completion-list nil nil nil nil
                            (cadr completion-list)))))
            (when (and to-insert (region-active-p))
              ;; The currently highlighted section is to be replaced by the yank
              (delete-region (region-beginning) (region-end)))
            (insert to-insert))

(defun modo-byte-compile-core ()
  "Byte compiles the modo core, i.e. elisp files found in
`modo-core-dir'."
  (interactive)
  (byte-recompile-directory modo-core-dir 0 t))

(defun modo-byte-compile-modules ()
  "Byte compiles the modo modules, i.e. elisp files found in
`modo-modules-dir'."
  (interactive)
  (byte-recompile-directory modo-modules-dir 0 t))

(defun modo-byte-compile-all ()
  "Byte compiles all of modo emacs."
  (interactive)
  (modo-byte-compile-core)
  (modo-byte-compile-modules)
  (byte-compile-file user-init-file))

(defmacro modo-install-search-engine (engine-name engine-url ex-prompt)
  "Given the ENGINE-NAME of a search engine, the ENGINE-URL to
construct a search prompt, and an EX-PROMPT, construct an ex
command to search the given search engine."
  (let ((defun-symbol (intern (format "modo--ex-%s" engine-name))))
  `(progn
     (evil-define-command ,defun-symbol (prompt)
       ,(format "Search %s with ex query." engine-name)
       (interactive "<a>")
       (modo-url-search ,engine-url prompt))
     (evil-ex-define-cmd ,ex-prompt ',defun-symbol))))

(modo-install-search-engine "google" "https://www.google.com/search?q=" "google")
(modo-install-search-engine "duck-duck-go" "https://www.duckduckgo.com/?t=lm&q=" "ddg")
(modo-install-search-engine "github" "https://www.github.com/search?q=" "ghub")
(modo-install-search-engine "wikipedia" "https://en.wikipedia.org/w/index.php?search=" "wiki")

(provide 'modo-utils)
;;; modo-utils.el ends here

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
  (let ((buf (window-buffer))
        (display-buffer-overriding-action
         '(display-buffer-same-window (reusable-frames . t))))
    (switch-to-buffer
     (cl-find-if (lambda (b) (not (eq b buf)))
                 (mapcar 'car (window-prev-buffers))))))

(defun modo-get-faces (pos)
  "Get a list of active faces at position POS."
  (flatten-tree (list
                 (get-char-property pos 'read-face-name)
                 (get-char-property pos 'face)
                 (plist-get (text-properties-at pos) 'face))))

(defun modo-show-faces (pos)
  "Show the active faces at the current point."
  (interactive "d")
  (message (format "%s" (modo-get-faces pos))))

(defun modo-kill-non-default-buffers ()
  "Kill all buffers except the startup ones."
  (interactive)
  ;; I don't currently have a dashboard, but historically I change my mind...
  (let ((preserved-buffers '("*dashboard*" "*Messages*" "*scratch*")))
    (mapc (lambda (buffer)
            (unless (member (buffer-name buffer) preserved-buffers)
              (kill-buffer buffer)))
          (buffer-list))))

(straight-use-package 'transpose-frame)
(defun modo-rotate-frame-wrapper (arg)
  "Rotate the current frame.

Without prefix arguments, rotates the frame 90 degrees clockwise.
With one prefix argument, rotates the frame 90 degrees
counter-clockwise. With two prefix arguments, rotates the frame
180 degrees."
  (interactive "P")
  (cond
   ((equal arg '(4))
    (rotate-frame-anticlockwise))
   ((equal arg '(16))
    (rotate-frame))
   (t
    (rotate-frame-clockwise))))

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

(defun modo-force-byte-compile-package (package)
  "Recompiles a package build directory.

This is here because unfortunately some packages do not compile
cleanly with straight. Ideally this should be fixed upstream, but
until such time, we have this as a workaround."
  (interactive (list (straight--select-package "Select package"
                                               #'straight--installed-p)))
  (byte-recompile-directory (straight--build-dir package)
                            0 t t))

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

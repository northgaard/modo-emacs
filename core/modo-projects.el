;;; modo-projects.el --- project handling -*- lexical-binding: t -*-
;;; Commentary:

;; Handling of projects powered by project.el.

;;; Code:

(use-package project
  :init
  (setq project-list-file (concat modo-cache-dir "projects")
        project-compilation-buffer-name-function #'project-prefixed-buffer-name)
  :general
  (modo-define-leader-key :keymaps 'override
    "p" '(:keymap project-prefix-map :wk "project"))
  (general-define-key :keymaps 'project-prefix-map
                      "a" 'ff-find-other-file
                      "R" 'modo-revert-all-project-file-buffers)
  :config
  (setq project-switch-commands (seq-remove (lambda (item)
                                             (string-equal (cadr item) "VC-Dir"))
                                           project-switch-commands)))

(use-package vc-git
  :config
  ;; Same as default, with added "-I" to ignore binary files
  (setq vc-git-grep-template "git --no-pager grep -n -I <C> -e <R> -- <F>"))

(evil-define-command vc-git-grep-ex-command (prompt)
  "Git grep in the current repository with an ex query."
  (interactive "<a>")
  (require 'vc-git)
  (vc-git-grep prompt "*" (vc-root-dir)))
(evil-ex-define-cmd "gg[rep]" #'vc-git-grep-ex-command)

(defvar-local modo-file-jump-directory #'file-name-directory
  "Function determining the root directory for file jumps. Should
take a file name and return a directory.")

(defvar-local modo--current-jump-directory nil)

(defun modo-revert-all-project-file-buffers ()
  "Reverts all file visiting project buffers."
  (interactive)
  (require 'project)
  (when (yes-or-no-p "Revert all file visiting project buffers?")
    (dolist (buffer (project-buffers (project-current)))
      (when (buffer-file-name buffer)
        (with-current-buffer buffer
          (revert-buffer nil 'noconfirm))))))

;; -- Some extras nixed from projectile --
(defvar project-generic-command
  (cond
   ;; we prefer fd over find
   ((executable-find "fd")
    "fd . -0 --type f --color=never --strip-cwd-prefix")
   ;; fd's executable is named fdfind is some Linux distros (e.g. Ubuntu)
   ((executable-find "fdfind")
    "fdfind . -0 --type f --color=never --strip-cwd-prefix")
   ;; with find we have to be careful to strip the ./ from the paths
   ;; see https://stackoverflow.com/questions/2596462/how-to-strip-leading-in-unix-find
   (t (format "%s . -type f | cut -c3- | tr '\\n' '\\0'" find-program))))

(defun project-files-via-ext-command (root command)
  "Get a list of relative file names in the project ROOT by executing COMMAND.

If `command' is nil or an empty string, return nil.
This allows commands to be disabled.

Only text sent to standard output is taken into account."
  (when (stringp command)
    (let ((default-directory root))
      (with-temp-buffer
        (shell-command command t "*project-files-errors*")
        (let ((shell-output (buffer-substring (point-min) (point-max))))
          (split-string (string-trim shell-output) "\0" t))))))
;; -- End of projectile extras --

(defun modo-file-jump ()
  "Jump to a file from the current one, selecting from a list of
all files in the directory returned by `modo-file-jump-directory'
for the current file."
  (interactive)
  (if-let ((file (buffer-file-name (current-buffer))))
      (let* ((modo--current-jump-directory (funcall modo-file-jump-directory file))
             (dir modo--current-jump-directory)
             (cp (project-current))
             (candidates (if cp
                             (mapcar (lambda (file)
                                       (file-relative-name file dir))
                                     (project-files cp (list dir)))
                           (project-files-via-ext-command dir project-generic-command))))
        (let ((file (completing-read
                     (format "Find file in %s: " dir)
                     (lambda (input predicate action)
                       (if (eq action 'metadata)
                           '(metadata (display-sort-function . identity)
                                      (cycle-sort-function . identity)
                                      (category . file-jump))
                         (complete-with-action action candidates input predicate))))))
          (find-file (expand-file-name file dir))))
    (user-error "Not a file visiting buffer!")))

(provide 'modo-projects)
;;; modo-projects.el ends here

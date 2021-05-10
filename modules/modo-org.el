;;; modo-org.el --- org-mode -*- lexical-binding: t -*-
;;; Commentary:

;; My life in plain text and elisp hacks.

;;; Code:

(defcustom modo-org-root-dir "~/org"
  "Directory for org files, typically stored in some shared folder,
i.e. with Dropbox."
  :type 'directory
  :group 'modo-emacs)

(defconst modo--org-inbox "inbox.org")
(defconst modo--org-work "work.org")
(defconst modo--org-personal "personal.org")
(defconst modo--org-incubator "incubator.org")

(defun modo-get-org-file (file)
  "Returns the full path to org file FILE in `modo-org-root-dir'."
  (expand-file-name file modo-org-root-dir))

(defvar modo--org-files-at-root
  (thunk-delay (-filter (lambda (file) (f-ext-p file "org"))
                        (f-entries modo-org-root-dir))))

(defun modo-org-mode-setup ()
  (org-superstar-mode 1)
  (evil-org-mode 1)
  (evil-normalize-keymaps))

(defun modo-find-org-file (filename)
  "Find file FILENAME, using `modo-org-root-dir' as the base
directory for completion."
  (interactive
   (list (read-file-name "Select file: "
                         modo-org-root-dir
                         (car (thunk-force modo--org-files-at-root)))))
   (find-file filename)
   (run-hooks 'find-file-hook))

(straight-use-package '(org :depth 1))
(use-package org
  :defer 10
  :hook (org-mode . modo-org-mode-setup)
  :general
  (:keymaps 'org-mode-map
            "M-s" 'consult-outline
            "C-M-<return>" 'org-insert-subheading)
  (modo-define-leader-key :keymaps 'override
    "fo" '(modo-find-org-file :wk "find-org-file"))
  (modo-define-major-leader-key :keymaps 'org-mode-map
    "c" 'org-ctrl-c-ctrl-c
    "r" 'org-refile
    "d" 'org-deadline
    "s" 'org-schedule
    "t" 'org-todo
    "T" 'org-todo-force-note
    "q" 'org-set-tags-command
    "o" 'org-open-at-point
    "." 'org-time-stamp
    "!" 'org-time-stamp-inactive)
  :init
  ;; Make double extra sure that the built-in org-version is not loaded
  (when (featurep 'org-version)
    (unload-feature 'org-version t))
  (setq org-modules '(org-habit org-id org-protocol org-timer))
  :config
  (modo-define-leader-key
    :keymaps 'override
    "nb" 'org-narrow-to-block
    "ne" 'org-narrow-to-element
    "ns" 'org-narrow-to-subtree)
  (require 'org-edna)
  (require 'evil-org)
  (setq org-startup-indented t
        org-src-tab-acts-natively t
        org-src-fontify-natively t
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-fontify-done-headline t
        org-fontify-whole-heading-line t
        org-log-done 'time
        org-agenda-start-with-log-mode t
        org-log-into-drawer t
        org-refile-use-cache t
        org-hide-emphasis-markers t
        org-hidden-keywords '(title)
        org-ellipsis " ▾")
    ;; Set face height for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :height (cdr face)))
  (set-face-attribute 'org-document-title nil
                      :height 1.7
                      :foreground 'unspecified
                      :inherit 'org-level-1)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(p!)" "WAITING(w@/!)"
                    "|" "DONE(d!)" "CANCELED(c@/@)")))
  ;; TODO: Need a better way to reference theme colors
  (setq org-todo-keyword-faces '(("NEXT" . 'org-level-1)
                                 ("IN-PROGRESS" . 'org-level-2)
                                 ("WAITING" . 'org-level-2)))
  (setq org-agenda-files (thunk-force modo--org-files-at-root))
  (setq org-capture-templates `(("i" "GTD item"
                                 entry (file ,(modo-get-org-file modo--org-inbox))
                                 "* %?\n%U\n\n  %i"
                                 :kill-buffer t)
                                ("l" "GTD item with link to where you are in Emacs now"
                                 entry (file ,(modo-get-org-file modo--org-inbox))
                                 "* %?\n%U\n\n  %i\n  %a"
                                 :kill-buffer t)))
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets `((,(modo-get-org-file modo--org-work) :maxlevel . 3)
                             (,(modo-get-org-file modo--org-personal) :maxlevel . 3)
                             (,(modo-get-org-file modo--org-incubator) :maxlevel . 2)))
  (defun org-todo-force-note ()
    "Like `org-todo', but forces a note on the state change."
    (interactive)
    (let ((org-log-done 'note))
      (call-interactively 'org-todo)))
  ;; Periodically save org buffers
  (defun modo--org-save-all-org-buffers (&rest args)
    "Like `org-save-all-org-buffers', but quiet and non-interactive."
    (modo-quieten
     (org-save-all-org-buffers)))
  (run-with-idle-timer 60 t #'modo--org-save-all-org-buffers)
  (advice-add 'org-refile :after #'modo--org-save-all-org-buffers)
  (doom-themes-org-config))

(straight-use-package 'org-edna)
(use-package org-edna
  :custom (org-edna-use-inheritance t)
  :config
  (org-edna-mode 1))

(straight-use-package 'evil-org)
(use-package evil-org
  :commands (evil-org-mode)
  :custom (evil-org-key-theme '(navigation insert textobjects additional))
  :config
  (evil-org-set-key-theme)
  (defun avy-org-goto-header ()
    "Jump to an org header at any level."
    (interactive)
    (avy-jump "^\\*+ "))
  (evil-define-key '(motion normal visual) 'evil-org-mode
    "gs" #'avy-org-goto-header
    "gS" #'avy-org-goto-heading-timer))

(use-package org-capture
  :general
  (modo-define-leader-key :keymaps 'override
    "c" 'org-capture)
  (modo-define-major-leader-key :keymaps 'org-capture-mode-map
    "c" 'org-capture-finalize
    "r" 'org-capture-refile
    "q" 'org-capture-kill)
  :hook (org-capture-mode . evil-insert-state))

(use-package org-agenda
  :general
  (modo-define-leader-key :keymaps 'override
    "a" 'org-agenda)
  :config
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t)
  (defun org-agenda-todo-force-note ()
    "Like `org-agenda-todo', but forces a note on the state change."
    (interactive)
    (let ((org-log-done 'note))
      (call-interactively 'org-agenda-todo)))
  (require 'evil-org-agenda))

(use-package evil-org-agenda
  :hook (org-agenda-finalize . evil-normalize-keymaps)
  :config
  (evil-org-agenda-set-keys)
  (general-define-key
   :states 'motion
   :keymaps 'org-agenda-mode-map
   "T" 'org-agenda-todo-force-note))

(use-package org-archive
  :custom (org-archive-save-context-info '(time olpath category todo itags)))

(straight-use-package '(org-html-themes :type git
                                        :host github
                                        :repo "fniessen/org-html-themes"
                                        :build nil))
(use-package ob-lob
  :config
  (org-babel-lob-ingest (concat modo-modules-dir "org-babel-library.org")))

(straight-use-package 'org-superstar)
(use-package org-superstar
  :custom
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●" "○" "●"))
  (org-superstar-item-bullet-alist '((?* . ?•)
                                     (?+ . ?➤)
                                     (?- . ?→))))

(provide 'modo-org)
;;; modo-org.el ends here

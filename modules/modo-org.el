;;; modo-org.el --- org-mode -*- lexical-binding: t -*-
;;; Commentary:

;; My life in plain text and elisp hacks.

;;; Code:

(defcustom modo-org-root-dir "~/org"
  "Directory for org files, typically stored in some shared folder,
i.e. with Dropbox.")

(defvar modo--agenda-tab-dispatch-fold-faces
  '(org-agenda-structure org-super-agenda-header error))

(defun modo-get-org-file (file)
  "Returns the full path to org file FILE in `modo-org-root-dir'."
  (expand-file-name file modo-org-root-dir))

(defun modo-org-mode-setup ()
  (org-bullets-mode 1)
  (evil-org-mode 1)
  (evil-normalize-keymaps))

(defun modo-find-org-file (filename)
  "Find file FILENAME, using `modo-org-root-dir' as the base
directory for completion."
  (interactive
   (list (read-file-name "Select file: "
                         modo-org-root-dir
                         (file-name-nondirectory
                          (modo-get-org-file "inbox.org")))))
   (find-file filename)
   (run-hooks 'find-file-hook))

;; This recipe ensures that org-plus-contrib satisfies any package
;; that may depend on org. See straight.el issue #352
(straight-use-package
 '(org-plus-contrib
   :repo "https://code.orgmode.org/bzg/org-mode.git"
   :local-repo "org"
   :files (:defaults "contrib/lisp/*.el")
   :includes (org)))

(use-package org
  :hook (org-mode . modo-org-mode-setup)
  :general
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
  ;; Now load the fix
  (straight--fix-org-function "org")
  (setq org-modules '(org-habit org-id org-protocol org-timer))
  :config
  (modo-define-leader-key
    :keymaps 'override
    "nb" 'org-narrow-to-block
    "ne" 'org-narrow-to-element
    "ns" 'org-narrow-to-subtree)
  (require 'evil-org)
  (setq org-startup-indented t
        org-src-tab-acts-natively t
        org-src-fontify-natively t
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-fontify-done-headline t
        org-fontify-whole-heading-line t
        org-log-done 'time
        org-refile-use-cache t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(p)" "WAITING(w)"
                    "|" "DONE(d)" "CANCELED(c)")))
  ;; TODO: Need a better way to reference theme colors
  (setq org-todo-keyword-faces '(("NEXT" . 'org-level-1)
                                 ("IN-PROGRESS" . 'org-level-2)
                                 ("WAITING" . 'org-level-2)))
  (setq org-agenda-files (mapcar #'modo-get-org-file
                                 '("inbox.org" "work.org" "personal.org" "tickler.org")))
  (setq org-capture-templates `(("t" "Todo [inbox]" entry
                                 (file+headline ,(modo-get-org-file "inbox.org") "Tasks")
                                 "* TODO %?")
                                ("T" "Tickler" entry
                                 (file+headline ,(modo-get-org-file "tickler.org") "Tickler")
                                 "* %? \n %U")))
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets `((,(modo-get-org-file "work.org") :maxlevel . 3)
                             (,(modo-get-org-file "personal.org") :maxlevel . 3)
                             (,(modo-get-org-file "someday.org") :level . 1)
                             (,(modo-get-org-file "tickler.org") :maxlevel . 2)))
  (defun org-todo-force-note ()
    "Like `org-todo', but forces a note on the state change."
    (interactive)
    (let ((org-log-done 'note))
      (call-interactively 'org-todo)))
  ;; Periodically save org buffers
  (defun modo--org-save-all-org-buffers ()
    "Like `org-save-all-org-buffers', but quiet and non-interactive."
    (cl-letf (((symbol-function 'message) #'ignore))
             (org-save-all-org-buffers)))
  (run-with-idle-timer 60 t #'modo--org-save-all-org-buffers))

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
  (general-define-key :states '(motion normal visual)
                      :keymaps 'evil-org-mode-map
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

(defun modo--org-agenda-count-group-items ()
  ;; TODO: Call recursively to count items in top level headers
  (let ((cind (current-indentation))
        (items 0))
    (save-excursion
      (forward-line)
      (while (and (not (eq cind (current-indentation)))
                  (not (eobp)))
        (when (org-find-text-property-in-string 'todo-state
                                                (thing-at-point 'line))
          (setq items (+ items 1)))
        (forward-line)))
    items))

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
  (defun modo-org-agenda-tab-dispatch ()
    ;; TODO: document this
    ""
    (interactive)
    (save-excursion
      (back-to-indentation)
      (let ((faces (modo-get-faces (point))))
        (if (-intersection
             modo--agenda-tab-dispatch-fold-faces
             faces)
            (progn
              (let ((origami-fold-replacement
                     (if (not (memq 'org-agenda-structure faces))
                         (let ((num-items (modo--org-agenda-count-group-items)))
                           (format "... (%s %s)"
                                   num-items (modo-pluralize num-items "item" "items")))
                       "...")))
              (call-interactively 'origami-toggle-node)))
          (call-interactively 'org-agenda-goto)))))
  (require 'evil-org-agenda)
  (require 'org-super-agenda)
  (setq org-agenda-custom-commands '(("p" "Personal agenda view"
                                      ((agenda "" ((org-agenda-span 'day)
                                                   (org-super-agenda-groups
                                                    '((:name "Today"
                                                             :time-grid t
                                                             :date today
                                                             :scheduled today
                                                             :order 1)
                                                      (:name "Missed schedule"
                                                             :header-face error
                                                             :scheduled past
                                                             :order 0)
                                                      (:name "Due today"
                                                             :deadline today
                                                             :order 2)
                                                      (:name "Overdue"
                                                             :header-face error
                                                             :deadline past
                                                             :order 0)
                                                      (:name "Due soon"
                                                             :deadline future
                                                             :order 3)))))
                                       (alltodo "" ((org-agenda-overriding-header "Tasks")
                                                    (org-super-agenda-groups
                                                     '((:discard (:scheduled t
                                                                  :deadline t
                                                                  :date t))
                                                       (:auto-parent t))))))
                                      ((org-agenda-compact-blocks nil)
                                       (org-agenda-block-separator "")
                                       (org-agenda-files (mapcar #'modo-get-org-file
                                                                 '("personal.org")))))
                                     ("w" "Work agenda view"
                                      ((agenda "" ((org-agenda-span 'day)
                                                   (org-super-agenda-groups
                                                    '((:name "Today"
                                                             :time-grid t
                                                             :date today
                                                             :scheduled today
                                                             :order 1)
                                                      (:name "Missed schedule"
                                                             :header-face error
                                                             :scheduled past
                                                             :order 0)
                                                      (:name "Due today"
                                                             :deadline today
                                                             :order 2)
                                                      (:name "Overdue"
                                                             :header-face error
                                                             :deadline past
                                                             :order 0)
                                                      (:name "Due soon"
                                                             :deadline future
                                                             :order 3)))))
                                       (alltodo "" ((org-agenda-overriding-header "Tasks")
                                                    (org-super-agenda-groups
                                                     '((:discard (:scheduled t
                                                                  :deadline t
                                                                  :date t))
                                                       (:auto-parent t))))))
                                      ((org-agenda-compact-blocks nil)
                                       (org-agenda-block-separator "")
                                       (org-agenda-files (mapcar #'modo-get-org-file
                                                                 '("work.org")))))
                                     ("c" "Prioritized agenda view"
                                      ((agenda "" ((org-agenda-span 'day)
                                                   (org-super-agenda-groups
                                                    '((:name "Today"
                                                             :time-grid t
                                                             :date today
                                                             :scheduled today
                                                             :order 1)
                                                      (:name "Missed schedule"
                                                             :header-face error
                                                             :scheduled past
                                                             :order 0)
                                                      (:name "Due today"
                                                             :deadline today
                                                             :order 2)
                                                      (:name "Overdue"
                                                             :header-face error
                                                             :deadline past
                                                             :order 0)
                                                      (:name "Due soon"
                                                             :deadline future
                                                             :order 3)))))
                                       (alltodo "" ((org-agenda-overriding-header "Tasks")
                                                    (org-super-agenda-groups
                                                     '((:discard (:scheduled t
                                                                  :deadline t
                                                                  :date t))
                                                       (:name "High-priority tasks"
                                                              :priority "A"
                                                              :order 0)
                                                       (:name "Work"
                                                              :tag "work"
                                                              :order 1)
                                                       (:name "Personal"
                                                              :tag "personal"
                                                              :order 2)
                                                       (:name "Inbox"
                                                              :tag "inbox"
                                                              :order 3))))))
                                      ((org-agenda-compact-blocks nil)
                                       (org-agenda-block-separator ""))))))

(use-package evil-org-agenda
  :hook (org-agenda-finalize . evil-normalize-keymaps)
  :config
  (evil-org-agenda-set-keys)
  (general-define-key
   :states 'motion
   :keymaps 'org-agenda-mode-map
   "T" 'org-agenda-todo-force-note
   "<tab>" 'modo-org-agenda-tab-dispatch))

(straight-use-package
 '(org-super-agenda :type git
                    :host github
                    :repo "alphapapa/org-super-agenda"
                    :fork (:repo "northgaard/org-super-agenda"
                                 :branch "header-face")))
(straight-use-package 'origami)

(use-package org-super-agenda
  :preface
  (defun modo--org-super-agenda-hook ()
    (origami-mode 1)
    (setq-local face-remapping-alist
                '((org-level-1 . org-super-agenda-header)
                  (org-level-2 . org-super-agenda-header)
                  (org-level-3 . org-super-agenda-header)
                  (org-level-4 . org-super-agenda-header)
                  (org-level-5 . org-super-agenda-header)
                  (org-level-6 . org-super-agenda-header)
                  (org-level-7 . org-super-agenda-header)
                  (org-level-8 . org-super-agenda-header))))
  :hook (org-agenda-mode . modo--org-super-agenda-hook)
  :config
  (org-super-agenda-mode 1)
  ;; When the header map is active, evil state is ignored
  ;; for some reason. This is the best work-around I've found
  ;; so far.
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (general-define-key :keymaps 'org-super-agenda-header-map
                      "q" 'org-agenda-quit
                      "<tab>" 'modo-org-agenda-tab-dispatch))

(use-package org-archive
  :custom (org-archive-save-context-info '(time olpath category todo itags)))

(straight-use-package 'org-bullets)
(use-package org-bullets
  :commands (org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("•")))

(provide 'modo-org)
;;; modo-org.el ends here

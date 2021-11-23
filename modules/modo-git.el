;;; modo-git.el --- it's magit, baby! -*- lexical-binding: t -*-
;;; Commentary:

;; Emacs interface to git based on magit.

;;; Code:

(use-package smerge-mode
  :after hydra
  :config
  (defhydra smerge-hydra
    (:color pink
            :hint nil
            :pre (if (not smerge-mode) (smerge-mode 1))
            ;; Disable smerge-mode if no conflicts remain
            :post (smerge-auto-leave))
    "
^Move^          ^Keep^               ^Diff^                 ^Other^
^^--------------^^-------------------^^---------------------^^-------
_j_: next       _b_ase               _<_: upper/base        _C_ombine
_k_: prev       _u_pper              _=_: upper/lower       _r_esolve
_C-j_: up       _l_ower              _>_: base/lower        _K_ill current
_C-k_: down     _a_ll                _R_efine
^^              _RET_: current       _E_diff
"
    ("C-j" next-line)
    ("C-k" previous-line)
    ("j" smerge-next)
    ("k" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("K" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue)))

(use-package ediff
  :config
  (setq ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-diff-options "-w")
  ;; https://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-changes-of-both-version
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference
                                                 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference
                                                 'B ediff-control-buffer))))
  (evil-collection-require 'ediff)
  ;; Mnemonic "All"
  (push '("A" . ediff-copy-both-to-C) evil-collection-ediff-bindings)
  (evil-collection-ediff-setup))

(straight-use-package 'git-modes)
(use-package git-modes)

(defvar modo--git-abbrev-length
  (thunk-delay (let ((abbrev (shell-command-to-string
                              "git config core.abbrev")))
                 (if (string-empty-p abbrev)
                     "7" ;; Default value
                   abbrev)))
  "The amount of characters git minimally uses for abbreviated hashes.")

(straight-use-package 'transient)
(use-package transient
  :config
  (setq transient-history-file (concat modo-cache-dir "transient/history.el")
        transient-levels-file (concat modo-cache-dir "transient/levels.el")
        transient-values-file (concat modo-cache-dir "transient/values.el")))

(defun modo-magit-status-straight-package (package)
  "Run magit status on PACKAGE installed by straight.el"
  (interactive (list (straight--select-package "Select package"
                                               #'straight--installed-p)))
  (magit-status (straight--repos-dir
                 (plist-get (gethash package straight--recipe-cache)
                            :local-repo))))

(straight-use-package 'magit)
(use-package magit
  :defer 20
  :init
  (with-eval-after-load 'embark
    (defun embark-magit-status (file)
      "Run `magit-status` on repo containing the embark target."
      (interactive "GFile: ")
      (magit-status (locate-dominating-file file ".git")))
    (general-define-key :keymaps 'embark-file-map
                        "v" 'embark-magit-status))
  (setq projectile-switch-project-action (lambda () (magit-status (projectile-project-root))))
  (modo-add-hook (git-commit-setup-hook)
    (modo-disable-auto-save-backup-locally)
    (evil-normalize-keymaps)
    (evil-insert-state))
  (modo-add-hook (magit-diff-visit-file-hook)
    (when smerge-mode
      (smerge-hydra/body)))
  (setq evil-collection-magit-want-horizontal-movement t)
  :general
  (modo-define-leader-key :keymaps 'override
    "s" 'magit-status)
  ;; Let's pretend with-editor-mode is a major mode
  (modo-define-major-leader-key :keymaps 'with-editor-mode-map
    "c" 'with-editor-finish
    "q" 'with-editor-cancel)
  (modo-define-leader-key
    :keymaps 'override
    "v" '(:ignore t :wk "version control")
    "vb" 'magit-blame
    "vc" 'magit-commit
    "vf" 'magit-find-file
    "vd" 'magit-dispatch
    "vl" 'magit-log-buffer-file
    "vS" 'magit-stage-file
    "vu" 'magit-unstage-file
    "vD" 'magit-file-dispatch
    "vw" 'magit-show-commit)
  (:states '(motion normal visual)
           :keymaps 'magit-log-mode-map
           "gs" #'avy-magit-log-goto-commit)
  (:keymaps 'projectile-command-map
            "y" 'modo-magit-status-straight-package)
  :config
  ;; Lazy initialization
  (defun avy-magit-log-goto-commit ()
    "Avy jump to an arbitrary commit in the magit-log view."
    (interactive)
    (avy-jump (format "^[a-zA-Z0-9]\\{%s,\\} [*|] "
                      (thunk-force modo--git-abbrev-length))))
  (when (string= "SPC" modo-leader)
    (general-define-key :keymaps 'magit-mode-map
                        modo-leader nil))
  ;; Use projectile to get known repositories
  (setq magit-repository-directories
        (mapcar (lambda (dir)
                  (cons (directory-file-name dir) 0))
                (cl-remove-if-not (lambda (project)
                                    (file-directory-p (concat project "/.git/")))
                                  (projectile-relevant-known-projects))))
  (setq magit-completing-read-function #'selectrum-completing-read
        magit-commit-diff-inhibit-same-window t)
  (evil-collection-require 'magit)
  (evil-collection-magit-setup))

(straight-use-package 'git-timemachine)
(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :general
  (modo-define-leader-key
    :keymaps 'override
    "vt" 'git-timemachine)
  :hook (git-timemachine-mode . evil-normalize-keymaps)
  :init
  (setq git-timemachine-mode-map (make-sparse-keymap))
  (general-define-key :keymaps 'git-timemachine-mode-map
                      :states '(normal visual motion)
                      "j" #'git-timemachine-show-next-revision
                      "k" #'git-timemachine-show-previous-revision
                      "f" #'git-timemachine-show-revision-fuzzy
                      "n" #'git-timemachine-show-nth-revision
                      "b" #'git-timemachine-blame
                      "c" #'git-timemachine-show-commit
                      "w" #'git-timemachine-kill-abbreviated-revision
                      "W" #'git-timemachine-kill-revision
                      "?" #'hydra-git-timemachine/body
                      "q" #'git-timemachine-quit)
  :config
  (defhydra hydra-git-timemachine
    (:color pink
            :hint nil
            :pre (unless git-timemachine-mode
                   (git-timemachine)))
            "
^Step^                     ^Find^                      ^Revision^
^^-------------------------^^--------------------------^^----------
_j_: next revision         _f_: fuzzy revision         _b_: blame
_k_: previous revision     _n_: nth revision           _c_: show commit
                                                   _w_: kill short hash
                                                   _W_: kill hash
"
            ("j" git-timemachine-show-next-revision)
            ("k" git-timemachine-show-previous-revision)
            ("f" git-timemachine-show-revision-fuzzy)
            ("n" git-timemachine-show-nth-revision)
            ("b" git-timemachine-blame :color blue)
            ("c" git-timemachine-show-commit :color blue)
            ("w" git-timemachine-kill-abbreviated-revision :color blue)
            ("W" git-timemachine-kill-revision :color blue)
            ("?" nil "close" :color blue)
            ("q" git-timemachine-quit "quit timemachine" :color blue)))

(defun modo--git-gutter-activate-maybe ()
  (let ((file-name (buffer-file-name (buffer-base-buffer))))
    (if (not (and file-name
                  (equal (vc-backend file-name) 'Git)))
        (add-hook 'after-save-hook #'modo--git-gutter-activate-maybe
                  nil 'local)
      (require 'git-gutter-fringe)
      (unless (memq major-mode git-gutter:disabled-modes)
        (git-gutter-mode 1)
        (remove-hook 'after-save-hook #'modo--git-gutter-activate-maybe 'local)))))

(defun modo--git-gutter-update (&rest _)
  (or (memq this-command '(git-gutter:stage-hunk
                           git-gutter:revert-hunk))
      inhibit-redisplay
      (when git-gutter-mode
        (git-gutter))))

(straight-use-package 'git-gutter)
(use-package git-gutter
  :general
  (modo-define-leader-key :keymaps 'override
    "vj" 'git-gutter:next-hunk
    "vk" 'git-gutter:previous-hunk
    "vs" 'git-gutter:stage-hunk
    "vR" 'git-gutter:revert-hunk
    "vC" 'modo-git-gutter-hunk-quick-commit)
  :init
  (add-hook 'find-file-hook #'modo--git-gutter-update)
  (add-hook 'find-file-hook #'modo--git-gutter-activate-maybe)
  (add-hook 'magit-diff-visit-file-hook #'modo--git-gutter-update)
  (add-hook 'magit-diff-visit-file-hook #'modo--git-gutter-activate-maybe)
  (add-hook 'modo-switch-window-hook #'modo--git-gutter-update)
  (defun modo-git-gutter-hunk-quick-commit ()
    (interactive)
    (require 'magit-git)
    (let ((staged-files (magit-staged-files))
          (hunkinfo (ignore-errors
                      (git-gutter:search-here-diffinfo git-gutter:diffinfos))))
      (unless hunkinfo
        (error "Not a changed hunk"))
      (when staged-files
        (error "Quick commit requires that no other changes are staged"))
      (let ((git-gutter:ask-p nil)
            (git-gutter:diffinfos (list hunkinfo))
            (commit-message (read-from-minibuffer "Commit message: ")))
        (git-gutter:stage-hunk)
        (message (magit-git-string "commit" "-m" commit-message)))))
  :custom
  (git-gutter:verbosity 0)
  (git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode org-mode))
  :config
  (defun modo--update-gutter-focus-in ()
    (cl-loop for frame in (frame-list)
             if (eq (frame-focus-state frame) t)
             return (git-gutter:update-all-windows)))
  (add-function :after after-focus-change-function #'modo--update-gutter-focus-in)
  (with-eval-after-load 'magit
    (advice-add #'magit-stage-file :after #'modo--git-gutter-update)
    (advice-add #'magit-unstage-file :after #'modo--git-gutter-update)))

(straight-use-package 'git-gutter-fringe)
(use-package git-gutter-fringe
  :config
  ;; standardize default fringe width
  (if (fboundp 'fringe-mode) (fringe-mode '4))
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom))

(provide 'modo-git)
;;; modo-git.el ends here

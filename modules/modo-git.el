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

(defun modo-magit-status-straight-package (package)
  "Run magit status on PACKAGE installed by straight.el"
  (interactive (list (straight--select-package "Select package"
                                               #'straight--installed-p)))
  (magit-status (straight--repos-dir
                 (plist-get (gethash package straight--recipe-cache)
                            :local-repo))))

(defun modo-magit-status-modo ()
  "Run magit status for modo itself."
  (interactive)
  (magit-status (file-name-directory user-init-file)))

(modo-define-leader-key :keymaps 'override
  "es" '(modo-magit-status-modo :wk "magit-status-modo")
  "ep" '(modo-magit-status-straight-package :wk "magit-status-straight-package"))

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
  (with-eval-after-load 'project
    (defun project-magit-status ()
      "Run `magit-status' in project."
      (interactive)
      (magit-status (project-root (project-current))))
    (add-to-list 'project-switch-commands
                 '(project-magit-status "Magit Status" ?s) 'append))
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
  (setq magit-completing-read-function #'completing-read-default
        magit-commit-diff-inhibit-same-window t)
  (defun modo--magit-maybe-toggle-line-numbers ()
    (when (derived-mode-p 'prog-mode 'text-mode)
      (display-line-numbers-mode 1)))
  (add-hook 'magit-find-file-hook #'modo--magit-maybe-toggle-line-numbers)
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

(defvar modo--diff-hl-disabled-modes
  '(fundamental-mode image-mode pdf-view-mode org-mode))

(defun modo--diff-hl-activate-maybe ()
  (let ((file-name (buffer-file-name (buffer-base-buffer))))
    (if (not (and file-name
                  (memq (vc-backend file-name) vc-handled-backends)))
        (add-hook 'after-save-hook #'modo--diff-hl-activate-maybe
                  nil 'local)
      (require 'diff-hl)
      (unless (memq major-mode modo--diff-hl-disabled-modes)
        (diff-hl-mode 1)
        (diff-hl-update)
        (remove-hook 'after-save-hook #'modo--diff-hl-activate-maybe 'local)))))

(straight-use-package 'diff-hl)
(use-package diff-hl
  :general
  (modo-define-leader-key :keymaps 'override
    "vj" 'diff-hl-next-hunk
    "vk" 'diff-hl-previous-hunk
    "vs" 'diff-hl-stage-current-hunk
    "vR" 'diff-hl-revert-hunk)
  :init
  (add-hook 'find-file-hook #'modo--diff-hl-activate-maybe)
  (with-eval-after-load 'magit
    (require 'diff-hl)
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(provide 'modo-git)
;;; modo-git.el ends here

;;; modo-completion.el --- Minibuffer completion -*- lexical-binding: t -*-
;;; Commentary:

;; Minibuffer completion and associated goodies, currently with selectrum.

;;; Code:

(straight-use-package 'prescient)
(use-package prescient
  :demand t
  :config
  (setq prescient-save-file (concat modo-cache-dir "prescient-save.el")
        prescient-aggressive-file-save t)
  (prescient-persist-mode 1))

;;; ------------
;;; Ideally I would like to gradually replace hydra with transient,
;;; but currently transient does not support invoking transient from
;;; the minibuffer (see transient issue #112). The code below shows
;;; how to implement the quick move hydra with transient, as a reference
;;; for the future.
;;; ------------
;; (transient-define-prefix tselectrum-quick-move ()
;;   :transient-suffix 'transient--do-stay
;;   :transient-non-suffix 'transient--do-warn
;;   [["Move"
;;     ("j" "next" selectrum-next-candidate)
;;     ("k" "previous" selectrum-previous-candidate)
;;     ("h" "back word" backward-kill-word)
;;     ("H" "back sexp" backward-kill-sexp)]
;;    ["Jump"
;;     ("n" "next page" selectrum-next-page)
;;     ("p" "previous page" selectrum-previous-page)
;;     ("q" "quit" transient-quit-one)]])

(defhydra selectrum-quick-move (:color pink)
  ("j" selectrum-next-candidate "next" :column "Move")
  ("k" selectrum-previous-candidate "previous")
  ("h" backward-kill-word "back word")
  ("H" backward-kill-sexp "back sexp")
  ("l" selectrum-insert-current-candidate "insert" :column "Jump")
  ("n" selectrum-next-page "next page")
  ("p" selectrum-previous-page "previous page")
  ("q" nil "quit"))

(straight-use-package 'selectrum)
(use-package selectrum
  :demand t
  :general
  (modo-define-leader-key "r" 'selectrum-repeat)
  (:keymaps 'selectrum-minibuffer-map
            "C-j" 'selectrum-next-candidate
            "C-k" 'selectrum-previous-candidate
            "C-n" 'selectrum-submit-exact-input
            "C-w" 'backward-kill-word
            "C-q" 'backward-kill-sexp
            "C-p" 'quoted-insert ; rebound from C-q
            "C-d" 'selectrum-next-page
            "C-b" 'selectrum-previous-page
            "C-v" 'selectrum-goto-end
            "M-v" 'selectrum-goto-beginning
            "M-j" 'next-history-element
            "M-k" 'previous-history-element
            "C-o" 'selectrum-quick-move/body)
  :config
  ;; Fully exit quick move hydra on C-g
  (add-hook 'minibuffer-exit-hook #'selectrum-quick-move/nil)
  (setq selectrum-count-style 'current/matches)
  (selectrum-mode 1))

(defvar modo-orderless-styles-alist
  '(("Default" . (orderless-prefixes orderless-initialism orderless-regexp))
    ("Literal" . (orderless-literal))
    ("Fuzzy" . (orderless-regexp orderless-flex)))
  "Alist of completion styles that can be cycled.")

(defvar modo--orderless-style-dispatchers-backup nil)
(defvar modo--orderless-style-cycle-counter 0)

(straight-use-package 'orderless)
(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless))
  (orderless-matching-styles
   (cdar modo-orderless-styles-alist))
  (orderless-style-dispatchers modo--orderless-style-dispatchers-backup)
  :general
  (:keymaps 'selectrum-minibuffer-map
            "C-l" 'modo-cycle-orderless-matching-style)
  :config
  (defun modo--reset-style-cycle-counter ()
    (setq modo--orderless-style-cycle-counter 0))
  (add-hook 'minibuffer-exit-hook #'modo--reset-style-cycle-counter)
  (setq orderless-skip-highlighting (lambda () selectrum-is-active)
        selectrum-highlight-candidates-function #'orderless-highlight-matches
        selectrum-refine-candidates-function #'orderless-filter)
  (defun modo-cycle-orderless-matching-style ()
    "During active completion, cycle through the matching styles
defined by `modo-orderless-styles-alist'."
    (interactive)
    (setq modo--orderless-style-cycle-counter
          (1+ modo--orderless-style-cycle-counter))
    (let* ((index (% modo--orderless-style-cycle-counter
                     (length modo-orderless-styles-alist)))
           (elem (elt modo-orderless-styles-alist index))
           (name (car elem))
           (styles (cdr elem)))
      (setq-local orderless-matching-styles styles)
      (if (string= name "Default")
          (setq-local orderless-style-dispatchers
                      modo--orderless-style-dispatchers-backup)
        (setq-local orderless-style-dispatchers nil))
      (message (format "Matching style: %s" name)))))

(straight-use-package 'selectrum-prescient)
(use-package selectrum-prescient
  :demand t
  :config
  (setq selectrum-prescient-enable-filtering nil)
  (selectrum-prescient-mode 1))

;; selectrum-info from the selectrum wiki
(let* ((selectrum-info-el (concat modo-modules-dir "selectrum-info.el"))
       (selectrum-info-elc (byte-compile-dest-file selectrum-info-el)))
  (unless (file-exists-p selectrum-info-elc)
    (byte-compile-file selectrum-info-el)))
(use-package selectrum-info
  :general
  (modo-define-leader-key :keymaps 'override
    "i" '(:ignore t :wk "info")
    "ii" 'selectrum-info
    "il" 'selectrum-info-elisp-manual
    "ie" 'selectrum-info-emacs-manual
    "io" 'selectrum-info-org-manual))

(straight-use-package 'marginalia)
(use-package marginalia
  :demand t
  :general
  (:keymaps 'selectrum-minibuffer-map
            "M-m" 'marginalia-cycle)
  :config
  (marginalia-mode 1)
  (advice-add #'marginalia-cycle :after
              (lambda () (selectrum-exhibit 'keep-selected))))

(straight-use-package 'embark)
(use-package embark
  :demand t
  :general
  (:keymaps 'selectrum-minibuffer-map
            "M-o" 'embark-act)
  :config
  (push '((lambda (buffer-name action)
            (with-current-buffer (get-buffer buffer-name)
              (derived-mode-p 'embark-collect-mode)))
           (display-buffer-reuse-window display-buffer-pop-up-window)
           (reusable-frames . t))
        display-buffer-alist)
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (kill-buffer which-key--buffer)
        (which-key--show-keymap
         (if (eq (caar targets) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (caar targets)
                   (embark--truncate-target (cdar targets))
                   (if (cdr targets) "…" "")))
         (if prefix (lookup-key keymap prefix) keymap)
         nil nil t))))
  (setq embark-indicator #'embark-which-key-indicator))

(straight-use-package 'avy-embark-collect)
(use-package avy-embark-collect
  :general
  (:states '(motion normal visual)
           :keymaps 'embark-collect-mode-map
           "o" 'avy-embark-collect-choose))

(straight-use-package 'consult)
(use-package consult
  :general
  (:keymaps 'override
            [remap isearch-forward] 'consult-line
            [remap switch-to-buffer] 'consult-buffer
            [remap recentf-open-files] 'consult-recent-file)
  (modo-define-leader-key :keymaps 'override
    "im" 'consult-man)
  (:states '(motion normal visual)
           "gp" 'consult-yank)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        consult-narrow-key (kbd "M-n"))
  :config
  (defun modo--consult-line-evil-ex (fn &rest args)
    (condition-case nil
        (apply fn args)
      (:success (when (and (bound-and-true-p evil-mode)
                           (eq evil-search-module 'evil-search))
                  (let ((cand (car consult--line-history)))
                    (add-to-history 'evil-ex-search-history cand)
                    (setq evil-ex-search-pattern (list cand t t)
                          evil-ex-search-direction 'forward)
                    (when evil-ex-search-persistent-highlight
                      (evil-ex-search-activate-highlight evil-ex-search-pattern)))))))
  (advice-add 'consult-line :around #'modo--consult-line-evil-ex)
  (setq consult-project-root-function #'projectile-project-root)
  (consult-customize
   consult-recent-file :preview-key nil))

(straight-use-package 'embark-consult)
(use-package embark-consult
  :demand t
  :after (embark consult)
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))

(provide 'modo-completion)
;;; modo-completion.el ends here

;;; modo-completion.el --- Minibuffer completion -*- lexical-binding: t -*-
;;; Commentary:

;; Minibuffer completion and associated goodies, currently with
;; vertico/orderless/marginalia/embark et al.

;;; Code:


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

(straight-use-package '(vertico :repo "minad/vertico"
                                :files (:defaults "extensions/*.el")))
(defhydra vertico-quick-move (:color pink)
  ("j" vertico-next "next" :column "Move")
  ("k" vertico-previous "previous")
  ("h" backward-kill-word "back word")
  ("H" backward-kill-sexp "back sexp")
  ("l" vertico-insert "insert" :column "Jump")
  ("n" vertico-scroll-up "next page")
  ("p" vertico-scroll-down "previous page")
  ("q" nil "quit"))

(use-package vertico
  :demand t
  :general
  (:keymaps 'vertico-map
            "C-j" 'vertico-next
            "C-k" 'vertico-previous
            "C-w" 'backward-kill-word
            "C-q" 'backward-kill-sexp
            "C-p" 'quoted-insert ; rebound from C-q
            "C-d" 'vertico-scroll-up
            "C-b" 'vertico-scroll-down
            "C-v" 'vertico-last
            "M-v" 'vertico-first
            "M-j" 'next-history-element
            "M-k" 'previous-history-element
            "C-o" 'vertico-quick-move/body)
  :config
  ;; Fully exit quick move hydra on C-g
  (add-hook 'minibuffer-exit-hook #'vertico-quick-move/nil)
  (setq vertico-cycle t)
  (vertico-mode 1))

(use-package vertico-repeat
  :init
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  :general
  (modo-define-leader-key
    :keymaps 'override
    "r" 'vertico-repeat))

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

;; completing-read-info adapted from the selectrum wiki
(let* ((cr-info-el (concat modo-modules-dir "completing-read-info.el"))
       (cr-info-elc (byte-compile-dest-file cr-info-el)))
  (unless (file-exists-p cr-info-elc)
    (byte-compile-file cr-info-el)))
(use-package completing-read-info
  :general
  (modo-define-leader-key :keymaps 'override
    "i" '(:ignore t :wk "info")
    "ii" 'completing-read-info
    "il" 'completing-read-info-elisp-manual
    "ie" 'completing-read-info-emacs-manual
    "io" 'completing-read-info-org-manual))

(straight-use-package 'marginalia)
(use-package marginalia
  :demand t
  :general
  (:keymaps 'vertico-map
            "M-m" 'marginalia-cycle)
  :config
  (marginalia-mode 1))

(straight-use-package 'embark)
(use-package embark
  :demand t
  :general
  (:keymaps 'vertico-map
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
  (setq embark-indicators '(embark-which-key-indicator
                            embark-highlight-indicator
                            embark-isearch-highlight-indicator)))

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
            [remap recentf-open-files] 'consult-recent-file
            [remap bookmark-jump] 'consult-bookmark)
  (modo-define-leader-key :keymaps 'override
    "im" 'consult-man)
  (:states '(motion normal visual)
           "gp" 'consult-yank-from-kill-ring)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        consult-narrow-key (kbd "M-n"))
  (autoload #'evil-collection-consult-mark "modes/consult/evil-collection-consult"
    "Jump to an evil marker in the current buffer." t)
  (evil-define-key '(normal visual motion) 'global
    "gM" 'evil-collection-consult-mark)
  :config
  (evil-collection-require 'consult)
  (evil-collection-consult-set-bindings)
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

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
            "M-k" 'previous-history-element)
  :config
  (setq selectrum-count-style 'current/matches)
  (selectrum-mode 1))

(straight-use-package 'orderless)
(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless))
  (orderless-matching-styles
   '(orderless-prefixes orderless-initialism orderless-regexp))
  :general
  (:keymaps 'selectrum-minibuffer-map
            "C-l" 'modo-match-components-literally)
  :config
  (setq orderless-skip-highlighting (lambda () selectrum-is-active)
        selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (defun modo-match-components-literally ()
    "Components match literally for the rest of the session."
    (interactive)
    (setq-local orderless-matching-styles '(orderless-literal)
                orderless-style-dispatchers nil)
    (message "Matching literally")))

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
    "ie" 'selectrum-info-elisp-manual
    "im" 'selectrum-info-emacs-manual
    "io" 'selectrum-info-org-manual))

(straight-use-package 'marginalia)
(use-package marginalia
  :demand t
  :general
  (:keymaps 'selectrum-minibuffer-map
            "M-m" 'marginalia-cycle)
  :config
  (marginalia-mode 1)
  (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (advice-add #'marginalia-cycle :after
              (lambda () (selectrum-exhibit 'keep-selected))))

(defhydra selectrum-quick-move (:color pink)
  ("j" selectrum-next-candidate "next" :column "Move")
  ("k" selectrum-previous-candidate "previous")
  ("h" backward-kill-word "back word")
  ("H" backward-kill-sexp "back sexp")
  ("l" selectrum-insert-current-candidate "insert" :column "Jump")
  ("n" selectrum-next-page "next page")
  ("p" selectrum-previous-page "previous page")
  ("q" nil "quit"))

(straight-use-package 'embark)
(use-package embark
  :demand t
  :general
  (:keymaps 'selectrum-minibuffer-map
            "M-o" 'embark-act
            "C-o" 'selectrum-quick-move/body)
  :config
  ;; Fully exit quick move hydra on C-g
  (add-hook 'minibuffer-exit-hook #'selectrum-quick-move/nil)
  (push '((lambda (buffer-name action)
            (with-current-buffer (get-buffer buffer-name)
              (derived-mode-p 'embark-collect-mode)))
           (display-buffer-reuse-window display-buffer-pop-up-window)
           (reusable-frames . t))
        display-buffer-alist)
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

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
  (:states '(motion normal visual)
           "gp" 'consult-yank)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
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
  (setq consult-project-root-function #'projectile-project-root
        consult-config '((consult-recent-file :preview-key nil))))

(straight-use-package 'embark-consult)
(use-package embark-consult
  :demand t
  :after (embark consult)
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))

(provide 'modo-completion)
;;; modo-completion.el ends here

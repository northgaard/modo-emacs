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
            "C-w" 'backward-kill-word
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
   '(orderless-regexp orderless-initialism orderless-flex))
  :config
  (setq orderless-skip-highlighting (lambda () selectrum-is-active)
        selectrum-highlight-candidates-function #'orderless-highlight-matches))

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

(straight-use-package 'embark)
(use-package embark
  :demand t
  :general
  (:keymaps 'selectrum-minibuffer-map
            "M-o" 'embark-act)
  :config
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(straight-use-package 'consult)
(use-package consult
  :general
  (:keymaps 'override
            [remap isearch-forward] 'consult-line
            [remap switch-to-buffer] 'consult-buffer
            [remap recentf-open-files] 'consult-recent-file)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-project-root-function #'projectile-project-root
        consult-config '((consult-recent-file :preview-key nil))))

(straight-use-package 'embark-consult)
(use-package embark-consult
  :demand t
  :after consult
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))

(provide 'modo-completion)
;;; modo-completion.el ends here

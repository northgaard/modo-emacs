;; -*- lexical-binding: t -*-
(require 'modo-core (concat user-emacs-directory "core/modo-core"))

;;; evil mode
(modo-add-package-single goto-chg "evil/lib/goto-chg.el")
(modo-add-package evil "evil")
(modo-add-package-single evil-escape "evil-escape/evil-escape.el")
(use-package evil :demand t
  :init
  (setq evil-want-C-u-scroll t
	evil-want-visual-char-semi-exclusive t
	evil-want-Y-yank-to-eol t
	evil-magic t
	evil-echo-state t
	evil-indent-convert-tabs t
	evil-ex-search-vim-style-regexp t
	evil-insert-skip-empty-line t
	evil-symbol-word-search t
	shift-select-mode nil)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-ex-define-cmd "x" 'save-buffers-kill-terminal)
  (evil-ex-define-cmd "kill" 'save-buffers-kill-emacs)
  (use-package evil-escape :demand t
    :diminish evil-escape-mode
    :config
    (setq-default evil-escape-key-sequence "fd")
    (evil-escape-mode 1))
  (evil-mode 1))

(modo-add-package-single evil-exchange "evil-exchange/evil-exchange.el")
(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-install))

(modo-add-package-single evil-snipe "evil-snipe/evil-snipe.el")
(use-package evil-snipe
  :after evil
  :diminish evil-snipe-local-mode
  :config
  (setq evil-snipe-enable-highlight nil)
  (setq evil-snipe-enable-incremental-highlight nil)
  (setq evil-snipe-repeat-keys nil)
  (setq evil-snipe-smart-case t)
  (setq evil-snipe-scope 'line)
  (setq evil-snipe-spillover-scope 'visible)
  (setq evil-snipe-repeat-scope 'buffer)
  (evil-snipe-mode 1))

(modo-add-package-single avy "avy/avy.el")
(use-package avy
  :after evil-snipe
  :config
  (setq avy-timeout-seconds 0.5)
  (setq avy-background t))

(modo-add-package-single evil-easymotion "evil-easymotion/evil-easymotion.el")
(use-package evil-easymotion
  :after avy
  :config
  (evilem-default-keybindings "C-e") ;; Not sure about this binding
  (evilem-make-motion modo-easymotion-snipe-repeat #'evil-snipe-repeat
                        :bind ((evil-snipe-scope 'buffer)
                               (evil-snipe-enable-highlight)
                               (evil-snipe-enable-incremental-highlight)))
  (evilem-make-motion modo-easymotion-snipe-repeat-reverse
                        #'evil-snipe-repeat-reverse
                        :bind ((evil-snipe-scope 'buffer)
                               (evil-snipe-enable-highlight)
                               (evil-snipe-enable-incremental-highlight)))
  (evilem-make-motion modo-easymotion-find-repeat #'evil-repeat-find-char
                      :bind ((evil-cross-lines t)))
  (evilem-make-motion modo-easymotion-find-repeat-reverse
                        #'evil-repeat-find-char-reverse
                        :bind ((evil-cross-lines t)))
  (evilem-define (kbd "C-e s") 'evil-snipe-repeat
                 :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                 :bind ((evil-snipe-scope 'buffer)
                        (evil-snipe-enable-highlight)
                        (evil-snipe-enable-incremental-highlight)))
  (evilem-define (kbd "C-e S") 'evil-snipe-repeat-reverse
                 :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                 :bind ((evil-snipe-scope 'buffer)
                        (evil-snipe-enable-highlight)
                        (evil-snipe-enable-incremental-highlight))))

(modo-add-package-single evil-surround "evil-surround/evil-surround.el")
(use-package evil-surround
  :after evil-snipe
  :config
  (global-evil-surround-mode 1))

(modo-add-package evil-commentary "evil-commentary")
(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode 1))

;; Standard keybinds
(modo-define-leader-key "f" '(:ignore t :which-key "files")
                        "fs" 'save-buffer
                        "fd" '(modo-find-dotfile :which-key "find-dotfile")
                        "f <deletechar>" '(modo-delete-this-file
                                  :which-key "delete-this-file")
                        "fn" '(modo-rename-this-file-and-buffer
                               :which-key "rename-this-file-and-buffer")
                        "f#" '(modo-delete-auto-save-file
                               :which-key "delete-auto-save-file")
                        "b" '(:ignore t :which-key "buffers")
                        "bd" 'kill-this-buffer
                        "w" '(:ignore t :which-key "windows")
                        "ws" 'evil-window-split
                        "wv" 'evil-window-vsplit
                        "wc" 'evil-window-delete
                        "wm" 'delete-other-windows
                        "wd" 'delete-frame
                        "wf" 'make-frame
                        "wo" 'other-frame
                        "wu" 'winner-undo
                        "wr" 'winner-redo
                        "u" 'universal-argument
                        "m" '(modo-major-leader-command :which-key "major mode"))

;; Horrible hackery to get the repeat behavior I want with evil-snipe
;; TODO: Figure out a clean way to do this
(defvar modo--last-evil-find 'find
  "Saves the last find type operation used, either the symbol find
or the symbol snipe.")
(make-variable-buffer-local 'modo--last-evil-find)

(defun modo--set-last-find-evil (&rest r)
  (setq modo--last-evil-find 'find))
(defun modo--set-last-find-snipe (&rest r)
  (setq modo--last-evil-find 'snipe))

(defun modo-fs-repeat (&optional count)
  (interactive "p")
  (pcase modo--last-evil-find
    (`find (evil-repeat-find-char count))
    (`snipe (evil-snipe-repeat count))))

(defun modo-fs-repeat-reverse (&optional count)
  (interactive "p")
  (pcase modo--last-evil-find
    (`find (evil-repeat-find-char-reverse count))
    (`snipe (evil-snipe-repeat-reverse count))))

(defun modo-easymotion-fs-repeat (&optional count)
  (interactive "p")
  (pcase modo--last-evil-find
    (`find (modo-easymotion-find-repeat count))
    (`snipe (modo-easymotion-snipe-repeat count))))

(defun modo-easymotion-fs-repeat-reverse (&optional count)
  (interactive "p")
  (pcase modo--last-evil-find
    (`find (modo-easymotion-find-repeat-reverse count))
    (`snipe (modo-easymotion-snipe-repeat-reverse count))))

;; Store last find type
(advice-add #'evil-find-char :after #'modo--set-last-find-evil)
(advice-add #'evil-find-char-backward :after #'modo--set-last-find-evil)
(advice-add #'evil-find-char-to :after #'modo--set-last-find-evil)
(advice-add #'evil-find-char-to-backward :after #'modo--set-last-find-evil)
(advice-add #'evil-snipe-s :after #'modo--set-last-find-snipe)
(advice-add #'evil-snipe-S :after #'modo--set-last-find-snipe)
(advice-add #'evil-snipe-x :after #'modo--set-last-find-snipe)
(advice-add #'evil-snipe-X :after #'modo--set-last-find-snipe)

(setq evil-snipe-parent-transient-map
      (let ((map (make-sparse-keymap)))
        (define-key map ";" #'evil-snipe-repeat)
        (define-key map "," #'modo-major-leader-command)
        map))
(modo-define-major-leader-key "," #'modo-fs-repeat-reverse)
(general-define-key :states '(motion normal visual)
                    ";" #'modo-fs-repeat
                    "g;" #'modo-easymotion-fs-repeat
                    "g," #'modo-easymotion-fs-repeat-reverse
                    "g." #'goto-last-change
                    "g:" #'goto-last-change-reverse)

;; Info-mode
(general-define-key :states 'motion
                    :keymaps 'Info-mode-map
                    "SPC" nil ;; Get back leader
                    "j" 'Info-scroll-up
                    "k" 'Info-scroll-down
                    "h" 'Info-history-back
                    "l" 'Info-history-forward
                    "C-j" 'evil-next-line
                    "C-k" 'evil-previous-line
                    "C-h" 'evil-backward-char
                    "C-l" 'evil-forward-char
                    "gg" 'evil-goto-first-line
                    "G" 'evil-goto-line
                    "s" 'swiper)

;;; org mode
(modo-add-package org "org-mode/lisp")
(modo-add-package org-contribdir "org-mode/contrib/lisp")
(modo-add-package-single org-bullets "org-bullets/org-bullets.el")
(use-package org
  :config
  (setq org-startup-indented t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-fontify-natively t)
  (use-package org-bullets
    :commands (org-bullets-mode)
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    (setq org-bullets-bullet-list '("•")))
  (use-package org-drill :demand t
    :init
    (require 'cl) ;; Needs the outdated cl lib
    (setq org-id-locations-file (expand-file-name "org-id-locations" modo-cache-dir))
    (setq org-drill-add-random-noise-to-intervals-p t)
    (setq org-drill-learn-fraction 0.4)))

(modo-add-package-single evil-org "evil-org-mode/evil-org.el")
(use-package evil-org
  :after org
  :commands (evil-org-mode)
  :init
  (add-hook 'org-mode-hook (lambda () (evil-org-mode 1)))
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional)))

;;; ivy/ivy hydra/counsel/swiper
;; Ivy needs a more precise recipe because all four packages are in one repo
(quelpa `(ivy :fetcher file
              :path ,(concat modo-repo-dir "swiper")
              :files (:defaults
                      (:exclude "swiper.el" "counsel.el" "ivy-hydra.el")
                      "doc/ivy-help.org")))
(add-to-list 'package-selected-packages 'ivy)
(modo-add-package-single ivy-hydra "swiper/ivy-hydra.el")
(modo-add-package-single swiper "swiper/swiper.el")
(modo-add-package-single counsel "swiper/counsel.el")
(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :general
  (modo-define-leader-key "bb" 'ivy-switch-buffer
                          "r" 'ivy-resume)
  (:keymaps 'ivy-minibuffer-map
            "C-j" 'ivy-next-line
            "C-k" 'ivy-previous-line
            "M-j" 'ivy-next-history-element
            "M-k" 'ivy-previous-history-element)
  :config
  (use-package ivy-hydra
    :commands (hydra-ivy/body)))

(use-package counsel
  :general
  (:states '(normal insert visual emacs motion)
           "M-x" 'counsel-M-x)
  (modo-define-leader-key "ff" 'counsel-find-file
                          "fr" 'counsel-recentf
                          "fb" 'counsel-bookmark))

(use-package swiper
  :commands (swiper)
  :general
  (:states '(normal insert visual emacs motion)
           "C-s" 'swiper))

;;; elisp
(defun modo--elisp-extra-fontification ()
  "Fontify modo functions."
  (font-lock-add-keywords
   nil `(("\\(^\\|\\s-\\|,\\)(\\(\\(modo\\|\\+\\)[^) ]+\\)[) \n]" (2 font-lock-keyword-face)))))

(add-hook 'emacs-lisp-mode-hook #'modo--elisp-extra-fontification)

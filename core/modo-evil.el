;;; modo-evil.el --- the unholy union -*- lexical-binding: t -*-
;;; Commentary:

;; Make emacs like vim, but better (maybe).

;;; Code:

;;; Pure evil
(modo-add-package-single goto-chg "evil/lib/goto-chg.el")
(modo-add-package evil "evil")
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
  ;; Set cursor colors after theme is loaded
  (defun modo--init-cursors (&rest r)
    (setq evil-default-cursor (face-background 'cursor nil t)
          evil-normal-state-cursor 'box
          evil-emacs-state-cursor `(,(face-foreground 'warning) box)
          evil-intert-state-cursor 'bar
          evil-visual-state-cursor 'hollow))
  (advice-add #'load-theme :after #'modo--init-cursors)
  (evil-mode 1))

;;; evil-escape
(modo-add-package-single evil-escape "evil-escape/evil-escape.el")
(use-package evil-escape :demand t
  :after evil
  :diminish evil-escape-mode
  :config
  (setq-default evil-escape-key-sequence "fd")
  (evil-escape-mode 1))

;;; evil-exchange
(modo-add-package-single evil-exchange "evil-exchange/evil-exchange.el")
(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-install))

;;; evil-snipe
(modo-add-package-single evil-snipe "evil-snipe/evil-snipe.el")
(use-package evil-snipe
  :after evil
  :diminish evil-snipe-local-mode
  :config
  (setq evil-snipe-enable-highlight nil
        evil-snipe-enable-incremental-highlight nil
        evil-snipe-repeat-keys nil
        evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-spillover-scope 'visible
        evil-snipe-repeat-scope 'buffer)
  ;; Hack the keymap to prevent it override leader
  (setq evil-snipe-parent-transient-map
        (let ((map (make-sparse-keymap)))
          (define-key map ";" #'evil-snipe-repeat)
          (define-key map "," #'modo-major-leader-command)
          map))
  (setq evil-snipe-disabled-modes '(Info-mode magit-mode))
  (evil-snipe-mode 1))

;;; evil-easymotion
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

;;; evil-surround
(modo-add-package-single evil-surround "evil-surround/evil-surround.el")
(use-package evil-surround
  :after evil-snipe
  :config
  (global-evil-surround-mode 1))

;;; evil-commentary
(modo-add-package evil-commentary "evil-commentary")
(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode 1))

;;; Horrible hackery to get the repeat behavior I want with evil-snipe
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

;;; Keybinds
(modo-define-major-leader-key "," #'modo-fs-repeat-reverse)
(general-define-key :states '(motion normal visual)
                    ";" #'modo-fs-repeat
                    "g;" #'modo-easymotion-fs-repeat
                    "g," #'modo-easymotion-fs-repeat-reverse
                    "g." #'goto-last-change
                    "g:" #'goto-last-change-reverse)

(provide 'modo-evil)
;;; modo-evil.el ends here

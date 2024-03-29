;;; modo-evil.el --- the unholy union -*- lexical-binding: t -*-
;;; Commentary:

;; Make emacs like vim, but better (maybe).

;;; Code:

;;; Pure evil
(straight-use-package 'evil)
(use-package evil
  :demand t
  :general
  (:states '(motion normal visual)
           "SPC" nil
           "," nil)
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-visual-char-semi-exclusive t
        evil-want-Y-yank-to-eol t
        evil-magic t
        evil-echo-state t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        evil-insert-skip-empty-line t
        evil-symbol-word-search t
        shift-select-mode nil
        evil-undo-system 'undo-redo)
  (advice-add 'undo-only :around #'modo-with-quiet-message)
  (advice-add 'undo-redo :around #'modo-with-quiet-message)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-ex-define-cmd "x" 'save-buffers-kill-terminal)
  (evil-ex-define-cmd "kill" 'save-buffers-kill-emacs)
  (defun evil-ex-insert-at-point ()
    "Insert the symbol at point into the ex command buffer."
    (interactive)
    (let ((at-point
           (with-current-buffer (window-buffer (minibuffer-selected-window))
             (thing-at-point 'symbol 'no-properties))))
      (when at-point
        (insert at-point))))
  (general-define-key :keymap 'evil-ex-completion-map
                      "C-t" 'evil-ex-insert-at-point)
  ;; Set cursor colors after theme is loaded
  (defun modo--init-cursors (&rest _)
    (setq evil-default-cursor (face-background 'cursor nil t)
          evil-normal-state-cursor 'box
          evil-emacs-state-cursor `(,(face-foreground 'warning) box)
          evil-intert-state-cursor 'bar
          evil-visual-state-cursor 'hollow))
  (advice-add #'load-theme :after #'modo--init-cursors)
  ;; When browsing the jump list, always use the same window, this
  ;; makes it easy to explore code by having a "reference" window with
  ;; some initial file visiting buffer, and a second window initially
  ;; visiting the same file, from which we can then jump around using
  ;; go to definition etc.
  (defun modo--with-display-buffer-same-window (fn &rest args)
    (let ((switch-to-buffer-obey-display-actions t)
          (display-buffer-overriding-action
           '(display-buffer-same-window (reusable-frames . t))))
      (apply fn args)))
  (advice-add #'evil-jump-forward :around #'modo--with-display-buffer-same-window)
  (advice-add #'evil-jump-backward :around #'modo--with-display-buffer-same-window)
  (evil-mode 1))

;;; evil-escape
(straight-use-package 'evil-escape)
(use-package evil-escape
  :demand t
  :after evil
  :diminish evil-escape-mode
  :config
  (setq-default evil-escape-key-sequence "fd")
  (evil-escape-mode 1))

;;; evil-exchange
(straight-use-package 'evil-exchange)
(use-package evil-exchange
  :demand t
  :after evil
  :config
  (evil-exchange-install))

;;; evil-snipe
(straight-use-package 'evil-snipe)
(use-package evil-snipe
  :demand t
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
  ;; Hack the keymap to prevent it overriding leader
  (setq evil-snipe-parent-transient-map
        (let ((map (make-sparse-keymap)))
          (define-key map ";" #'evil-snipe-repeat)
          map))
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

;;; evil-easymotion
(straight-use-package 'evil-easymotion)
(use-package evil-easymotion
  :demand t
  :after avy
  :config
  (evilem-default-keybindings "C-e") ;; Not sure about this binding
  (let ((byte-compile-warnings nil))
    (evilem-make-motion modo-easymotion-snipe-repeat #'evil-snipe-repeat
                        :bind ((evil-snipe-scope 'buffer)
                               (evil-snipe-enable-highlight)
                               (evil-snipe-enable-incremental-highlight)))
    (evilem-make-motion modo-easymotion-snipe-repeat-reverse
                        #'evil-snipe-repeat-reverse
                        :bind ((evil-snipe-scope 'buffer)
                               (evil-snipe-enable-highlight)
                               (evil-snipe-enable-incremental-highlight)))
    (evilem-define (kbd "C-e s") 'evil-snipe-repeat
                   :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                   :bind ((evil-snipe-scope 'buffer)
                          (evil-snipe-enable-highlight)
                          (evil-snipe-enable-incremental-highlight)))
    (evilem-define (kbd "C-e S") 'evil-snipe-repeat-reverse
                   :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                   :bind ((evil-snipe-scope 'buffer)
                          (evil-snipe-enable-highlight)
                          (evil-snipe-enable-incremental-highlight)))))

;;; evil-surround
(straight-use-package 'evil-surround)
(use-package evil-surround
  :demand t
  :after evil-snipe
  :config
  (global-evil-surround-mode 1))

;;; evil-commentary
(straight-use-package 'evil-commentary)
(use-package evil-commentary
  :demand t
  :after evil
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode 1))

;;; evil-args
(straight-use-package 'evil-args)
(use-package evil-args
  :init
  (modo-add-hook (prog-mode-hook
                  :name "modo--setup-evil-args"
                  :transient t)
    (require 'evil-args)
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)))

;;; evil-indent-plus
(straight-use-package 'evil-indent-plus)
(use-package evil-indent-plus
  :init
  (modo-add-hook (prog-mode-hook
                  :name "modo--setup-evil-indent-plus"
                  :transient t)
    (require 'evil-indent-plus)
    (evil-indent-plus-default-bindings)))

;;; evil-matchit
(straight-use-package 'evil-matchit)
(use-package evil-matchit
  :init
  (modo-add-hook (prog-mode-hook
                  :name "modo--setup-evil-matchit"
                  :transient t)
    (require 'evil-matchit)
    (global-evil-matchit-mode 1)))

;;; evil-numbers
(straight-use-package '(evil-numbers :type git :host github :repo "juliapath/evil-numbers"))
(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
  :general
  (:states '(motion normal visual)
           "g+" 'evil-numbers/inc-at-pt
           "g-" 'evil-numbers/dec-at-pt
           "<kp-add>" 'evil-numbers/inc-at-pt
           "<kp-subtract>" 'evil-numbers/dec-at-pt))

;;; evil-collection
(straight-use-package '(evil-collection :fork "northgaard"
                                        :branch "modo-work"))
(use-package evil-collection
  :custom (evil-collection-key-blacklist `(,modo-leader)))

;;; Keybinds
(with-eval-after-load 'evil-snipe
  (modo--direct-major-leader-key "," #'evil-snipe-repeat-reverse))
(general-define-key :states '(motion normal visual)
                    ";" #'evil-snipe-repeat
                    "g;" #'modo-easymotion-snipe-repeat
                    "g," #'modo-easymotion-snipe-repeat-reverse
                    "g." #'goto-last-change
                    "g:" #'goto-last-change-reverse)

;; General purpose hydras
(defhydra hydra-quick-scroll (:color red
                                     :body-pre (modo-temporary-set
                                                hydra-hint-display-type 'message)
                                     :post (modo-temporary-reset))
  "Quickly scroll through buffer"
  ("j" evil-scroll-down "down")
  ("k" evil-scroll-up "up")
  ("n" evil-scroll-page-down "page down")
  ("p" evil-scroll-page-up "page up")
  ("g" beginning-of-buffer "top")
  ("G" end-of-buffer "bottom")
  ("R" (lambda () (interactive) (setq evil-scroll-count 0)) "reset scroll count")
  ("q" nil "quit")
  ("SPC" nil "quit"))

(provide 'modo-evil)
;;; modo-evil.el ends here

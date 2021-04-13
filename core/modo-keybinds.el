;;; modo-keybinds.el --- base keybinds -*- lexical-binding: t -*-
;;; Commentary:

;; Global keybinds as well as binds for built-in modes.

;;; Code:

;;; Global keybinds
(modo-define-leader-key
  :keymaps 'override
  "f" '(:ignore t :wk "files")
  "fs" 'save-buffer
  "ff" 'find-file
  "fr" 'recentf-open-files
  "fb" 'bookmark-jump
  "fd" '(modo-find-dotfile :wk "find-dotfile")
  "f <deletechar>" '(modo-delete-this-file
                     :wk "delete-this-file")
  "fn" '(modo-rename-this-file-and-buffer
         :wk "rename-this-file-and-buffer")
  "f#" '(modo-delete-auto-save-file
         :wk "delete-auto-save-file")
  "fc" '(modo-find-core-file
         :wk "find-core-file")
  "fm" '(modo-find-module-file
         :wk "find-module-file")
  "b" '(:ignore t :wk "buffers")
  "bb" 'switch-to-buffer
  "bd" 'kill-this-buffer
  "bD" '(modo-kill-non-default-buffers
         :wk "kill-non-default-buffers")
  "bl" 'list-buffers
  "by" 'bury-buffer
  "br" 'revert-buffer
  "bw" 'follow-mode
  "bc" '(clone-indirect-buffer :wk "clone")
  "H"  'help-command
  "w"  '(:ignore t :wk "windows")
  "ws" 'evil-window-split
  "wv" 'evil-window-vsplit
  "wc" 'evil-window-delete
  "wm" 'delete-other-windows
  "wd" 'delete-frame
  "wf" 'make-frame-command
  "wF" 'make-frame-on-monitor
  "wo" 'ace-window
  "wu" 'winner-undo
  "wr" 'winner-redo
  "wp" 'ace-swap-window
  "wt" '(modo-rotate-frame-wrapper
         :wk "rotate-frame")
  "wj" 'windmove-down
  "wk" 'windmove-up
  "wh" 'windmove-left
  "wl" 'windmove-right
  "wz" 'hydra-window-resize/body
  "n" '(:ignore t :wk "narrow")
  "nd" 'narrow-to-defun
  "nn" 'narrow-to-region
  "np" 'narrow-to-page
  "nw" 'widen
  "y" 'hydra-pause-resume
  "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3
  "4" 'winum-select-window-4
  "5" 'winum-select-window-5
  "6" 'winum-select-window-6
  "7" 'winum-select-window-7
  "8" 'winum-select-window-8
  "9" 'winum-select-window-9
  "0" 'winum-select-window-0-or-10
  "z" '(hydra-font-size/body :wk "font-size")
  "TAB" '(modo-alternate-buffer
          :wk "alternate-buffer")
  "SPC" '(hydra-quick-scroll/body :wk "quick-scroll"))

(when (fboundp 'modo-insert-primary)
  (general-define-key :states '(motion normal visual)
                      "gP" '(modo-insert-primary :wk "insert-primary")))

;;; Info-mode
(use-package info
  :config
  (evil-collection-require 'info)
  (evil-collection-info-setup)
  (evil-collection-define-key 'normal 'Info-mode-map
    "o" 'ace-link-info
    (kbd modo-leader) nil))

(use-package compile
  :config
  (evil-collection-require 'compile)
  (evil-collection-compile-setup)
  (dolist (keymap evil-collection-compile-maps)
    (evil-collection-define-key 'normal keymap
      "o" 'ace-link-compilation)))

(use-package grep
  :config
  (evil-collection-require 'grep)
  (evil-collection-grep-setup)
  (evil-collection-define-key 'normal 'grep-mode-map
    "o" 'ace-link-compilation))

;;; ag
(use-package ag
  :config
  (evil-collection-require 'ag)
  (evil-collection-ag-setup))

(provide 'modo-keybinds)
;;; modo-keybinds.el ends here

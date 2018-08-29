;;; modo-keybinds.el --- base keybinds -*- lexical-binding: t -*-
;;; Commentary:

;; Global keybinds as well as binds for built-in modes.

;;; Code:

;;; Global keybinds
(modo-define-leader-key "f" '(:ignore t :wk "files")
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
                        "bl" 'list-buffers
                        "by" 'bury-buffer
                        "br" 'revert-buffer
                        "w" '(:ignore t :wk "windows")
                        "ws" 'evil-window-split
                        "wv" 'evil-window-vsplit
                        "wc" 'evil-window-delete
                        "wm" 'delete-other-windows
                        "wd" 'delete-frame
                        "wf" 'make-frame-command
                        "wo" 'ace-window
                        "wu" 'winner-undo
                        "wr" 'winner-redo
                        "wp" 'ace-swap-window
                        "u" 'universal-argument
                        "1" 'select-window-1
                        "2" 'select-window-2
                        "3" 'select-window-3
                        "4" 'select-window-4
                        "5" 'select-window-5
                        "6" 'select-window-6
                        "7" 'select-window-7
                        "8" 'select-window-8
                        "9" 'select-window-9
                        "z" '(hydra-font-size/body :wk "font-size")
                        "Z" '(modo-load-theme :wk "load-theme")
                        "TAB" '(modo-alternate-buffer
                                :wk "alternate-buffer"))

;;; Info-mode
;; Contortions to reclaim the leader key
(define-key Info-mode-map [override-state] nil)
(when (string= "SPC" modo-leader)
  (general-define-key :states 'motion
                      :keymaps 'Info-mode-map
                      modo-leader nil))
;; Actually interesting keybindings
(general-define-key :states 'motion
                    :keymaps 'Info-mode-map
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
                    "s" 'isearch-forward
                    "r" 'isearch-backward)

(provide 'modo-keybinds)
;;; modo-keybinds.el ends here

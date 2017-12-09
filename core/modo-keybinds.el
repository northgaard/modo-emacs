;;; modo-keybinds.el --- base keybinds -*- lexical-binding: t -*-
;;; Commentary:

;; Global keybinds as well as binds for built-in modes.

;;; Code:

;;; Global keybinds
(modo-define-leader-key "f" '(:ignore t :which-key "files")
                        "fs" 'save-buffer
                        "ff" 'find-file
                        "fr" 'recentf-open-files
                        "fb" 'bookmark-jump
                        "fd" '(modo-find-dotfile :which-key "find-dotfile")
                        "f <deletechar>" '(modo-delete-this-file
                                  :which-key "delete-this-file")
                        "fn" '(modo-rename-this-file-and-buffer
                               :which-key "rename-this-file-and-buffer")
                        "f#" '(modo-delete-auto-save-file
                               :which-key "delete-auto-save-file")
                        "fc" '(modo-find-core-file
                               :which-key "find-core-file")
                        "fm" '(modo-find-module-file
                               :which-key "find-module-file")
                        "b" '(:ignore t :which-key "buffers")
                        "bb" 'switch-to-buffer
                        "bd" 'kill-this-buffer
                        "bl" 'list-buffers
                        "by" 'bury-buffer
                        "w" '(:ignore t :which-key "windows")
                        "ws" 'evil-window-split
                        "wv" 'evil-window-vsplit
                        "wc" 'evil-window-delete
                        "wm" 'delete-other-windows
                        "wd" 'delete-frame
                        "wf" 'make-frame
                        "wo" 'ace-window
                        "wu" 'winner-undo
                        "wr" 'winner-redo
                        "wp" 'ace-swap-window
                        "u" 'universal-argument
                        "TAB" '(modo-alternate-buffer
                                :which-key "alternate-buffer"))

;;; Info-mode
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
                    "s" 'isearch-forward
                    "r" 'isearch-backward)

(provide 'modo-keybinds)
;;; modo-keybinds.el ends here

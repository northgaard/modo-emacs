;;; modo-cpp.el --- C++ programming -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for programming in C++ (work in progress)

;;; Code:

;; Use a mirror of cmake-mode, cloning all of cmake just for a single
;; elisp file is a bit ridiculous.
(straight-use-package '(cmake-mode :type git
                                   :host github
                                   :repo "northgaard/cmake-mode"
                                   :files (:defaults)))
(use-package cmake-mode)

;; Build it like a hipster.
(straight-use-package 'meson-mode)

(defvar-local modo-c++-enable-clang-format-on-save t
  "Buffer local variable to determine whether to run clang-format
on the buffer before saving.")
(put 'modo-c++-enable-clang-format-on-save 'safe-local-variable #'booleanp)

(straight-use-package 'clang-format)
(use-package clang-format
  :commands (clang-format-region clang-format-buffer)
  :config
  (defun modo--clang-format-on-save ()
    (when (and modo-c++-enable-clang-format-on-save
               (member major-mode '(c-mode c++-mode glsl-mode)))
      (progn
        (when (locate-dominating-file "." ".clang-format")
          (clang-format-buffer))
        ;; Return nil, to continue saving
        nil)))
  (defun clang-format-defun (&optional style assume-file-name)
    "Use clang-format to format the current function."
    (interactive)
    (let (beg end)
      (save-excursion
        (beginning-of-defun-comments)
        (setq beg (point))
        (end-of-defun)
        (setq end (point)))
      (clang-format-region beg end style assume-file-name)))
  ;; NOTE This currently relies on diff-hl, which only updates on
  ;; save. Ideally we would like to have this work with the diff of
  ;; the current buffer state and the latest checked-in version, so
  ;; that it could be run in before-save-hook. But this method is
  ;; simple and works for now.
  (defun clang-format-hunks (&optional style assume-file-name)
    "Use clang-format to format all unstaged hunk in the current buffer."
    (interactive)
    (unless (bound-and-true-p diff-hl-mode)
      (user-error "diff-hl not enabled!"))
    (let ((overlay nil)
          (p (point-min)))
      (while (setq overlay (diff-hl-search-next-hunk nil p))
        (let ((start (overlay-start overlay))
              (end (overlay-end overlay)))
          (clang-format-region start end style assume-file-name)
          (setq p (1+ end))))))
  (when (executable-find clang-format-executable)
    (add-hook 'before-save-hook #'modo--clang-format-on-save)))

(defcustom modo-clangd-number-of-worker-threads 2
  "Number of worker threads allowed for clangd.")

;; TODO Generalize this into a macro for defining a specialized mode
;; for any language
(define-minor-mode lsp-c++-mode
  "Specialized minor mode for c++-mode with lsp."
  :keymap (make-sparse-keymap))

(modo-define-major-leader-key :keymaps 'lsp-c++-mode-map
  "g" '(:prefix-command lsp-c++-goto-command :wk "goto")
  "gg" 'lsp-find-definition
  "gd" 'lsp-find-declaration
  "gr" 'lsp-find-references
  "h" '(:prefix-command lsp-c++-help-command :wk "help")
  "hh" 'lsp-describe-thing-at-point
  "hs" 'lsp-signature-activate
  "s" '(:prefix-command lsp-c++-session-command :wk "session")
  "sd" 'lsp-describe-session
  "sr" 'lsp-workspace-restart
  "sq" 'lsp-workspace-shutdown
  "ss" 'lsp
  "r" '(:prefix-command lsp-c++-refactor-command :wk "refactor")
  "rr" 'lsp-rename
  "ro" 'lsp-organize-imports
  "a" 'lsp-execute-code-action
  "c" '(:prefix-command lsp-c++-consult-command :wk "consult")
  "ce" 'consult-flymake
  "cd" 'consult-lsp-diagnostics
  "cs" 'consult-lsp-symbols)
(general-define-key :keymaps 'lsp-c++-mode-map
                    [remap ff-find-other-file] 'lsp-clangd-find-other-file
                    [remap evil-goto-definition] 'lsp-find-definition
                    "M-s" 'consult-lsp-file-symbols)
(general-define-key :keymaps 'lsp-c++-mode-map
                    :states '(motion normal visual)
                    "gr" #'lsp-find-references)
(add-hook 'lsp-c++-mode-hook #'evil-normalize-keymaps)

(straight-use-package 'modern-cpp-font-lock)
(use-package c++-mode
  :custom
  (lsp-clients-clangd-args `("--clang-tidy" "--log=info" "--pretty" "--limit-results=500"
                             "--background-index" "--header-insertion=never"
                             ,(format "-j=%d" modo-clangd-number-of-worker-threads)))
  :general
  (modo-define-major-leader-key :keymaps 'c++-mode-map
    "f" '(:ignore t :wk "format")
    "fb" 'clang-format-buffer
    "fr" 'clang-format-region
    "fd" 'clang-format-defun
    "fv" 'clang-format-hunks
    "j" 'flymake-goto-next-error
    "k" 'flymake-goto-prev-error)
  :init
  (modo-add-hook (c++-mode-hook :name "modo--load-clang-format"
                                :transient t)
    (require 'clang-format))
  (modo-add-hook (c++-mode-hook :name "modo--c++-mode-setup")
    (modern-c++-font-lock-mode 1)
    (evil-normalize-keymaps)
    (push '(?< . ("<" . ">")) evil-surround-pairs-alist))
  (modo-add-hook (c++-mode-local-vars-hook :name "modo--c++-mode-local-vars-setup")
    (when modo-enable-lsp
      (lsp-deferred)
      (lsp-c++-mode 1)
      (setq-local lsp-enable-indentation nil
                  lsp-enable-on-type-formatting nil
                  company-idle-delay 0
                  company-minimum-prefix-length 1))))

;; Search cppreference.com
(modo-install-search-engine "cppreference" "https://en.cppreference.com/mwiki/index.php?title=Special%3ASearch&search=" "cpp[ref]")

;; QT
(straight-use-package 'qml-mode)

;; GLSL
(straight-use-package 'glsl-mode)

(provide 'modo-cpp)
;;; modo-cpp.el ends here

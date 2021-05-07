;;; modo-cpp.el --- C++ programming -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for programming in C++ (work in progress)

;;; Code:

(straight-use-package '(cmake-mode :depth 1))
(use-package cmake-mode)

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
  ;; NOTE This currently relies on git-gutter, which only updates on
  ;; save. Ideally we would like to have this work with the diff of
  ;; the current buffer state and the latest checked-in version, so
  ;; that it could be run in before-save-hook. But this method is
  ;; simple and works for now.
  (defun clang-format-hunks (&optional style assume-file-name)
    "Use clang-format to format all unstaged hunk in the current buffer."
    (interactive)
    (require 'git-gutter-fringe)
    (if (not git-gutter-mode)
        (message "git gutter not enabled!")
      (save-excursion
        (dolist (diffinfo git-gutter:diffinfos)
          (let (beg
                end
                (start-line (git-gutter-hunk-start-line diffinfo))
                (end-line (git-gutter-hunk-end-line diffinfo)))
            (goto-char (point-min))
            (forward-line (1- start-line))
            (setq beg (point))
            (forward-line (- end-line start-line))
            (end-of-line)
            (setq end (point))
            (clang-format-region beg end style assume-file-name))))))
  (when (executable-find clang-format-executable)
    (add-hook 'before-save-hook #'modo--clang-format-on-save)))

(defcustom modo-clangd-number-of-worker-threads 2
  "Number of worker threads allowed for clangd.")

(defvar-local modo-c++-enable-lsp nil
  "Buffer local variable to determine whether the opened C++ file
should use lsp-mode.")
(put 'modo-c++-enable-lsp 'safe-local-variable #'booleanp)
(cl-pushnew 'c++-mode modo-consult-lsp-modes)

(straight-use-package 'modern-cpp-font-lock)
(use-package c++-mode
  :custom
  (lsp-clients-clangd-args `("--clang-tidy" "--log=info" "--pretty" "--background-index"
                             ,(format "-j=%d" modo-clangd-number-of-worker-threads)))
  :general
  (modo-define-major-leader-key :keymaps 'c++-mode-map
    "f" '(:ignore t :wk "format")
    "fb" 'clang-format-buffer
    "fr" 'clang-format-region
    "fd" 'clang-format-defun
    "fv" 'clang-format-hunks
    "j" 'flycheck-next-error
    "k" 'flycheck-previous-error)
  :init
  (modo-add-hook (c++-mode-hook :name "modo--load-clang-format"
                                :transient t)
    (require 'clang-format))
  (modo-add-hook (c++-mode-hook :name "modo--c++-mode-setup")
    (modern-c++-font-lock-mode 1)
    (hs-minor-mode 1)
    (evil-normalize-keymaps))
  (modo-add-hook (c++-mode-local-vars-hook :name "modo--c++-mode-local-vars-setup")
    (when modo-c++-enable-lsp
      (lsp-deferred)
      (setq-local company-idle-delay 0
                  company-minimum-prefix-length 1))))

;; Search cppreference.com
(modo-install-search-engine "cppreference" "https://en.cppreference.com/mwiki/index.php?title=Special%3ASearch&search=" "cpp[ref]")

;; QT
(straight-use-package 'qml-mode)

(provide 'modo-cpp)
;;; modo-cpp.el ends here

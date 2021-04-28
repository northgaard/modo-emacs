;;; modo-cpp.el --- C++ programming -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for programming in C++ (work in progress)

;;; Code:

(straight-use-package 'cmake-mode)
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
  (when (executable-find clang-format-executable)
    (add-hook 'before-save-hook #'modo--clang-format-on-save)))

(defcustom modo-clangd-number-of-worker-threads 2
  "Number of worker threads allowed for clangd.")

(defvar-local modo-c++-enable-lsp nil
  "Buffer local variable to determine whether the opened C++ file
should use lsp-mode.")
(put 'modo-c++-enable-lsp 'safe-local-variable #'booleanp)

(straight-use-package 'modern-cpp-font-lock)
(use-package c++-mode
  :custom
  (lsp-clients-clangd-args `("--clang-tidy" "--log=info" "--pretty" "--background-index"
                             ,(format "-j=%d" modo-clangd-number-of-worker-threads)))
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

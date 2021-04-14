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

(straight-use-package 'modern-cpp-font-lock)
(use-package c++-mode
  :init
  (modo-add-hook (c++-mode-hook :name "modo--load-clang-format"
                                :transient t)
    (require 'clang-format))
  (modo-add-hook (c++-mode-hook :name "modo--c++-mode-setup")
    (modern-c++-font-lock-mode 1)
    (hs-minor-mode 1)
    (evil-normalize-keymaps)))

;; Search cppreference.com
(modo-install-search-engine "cppreference" "https://en.cppreference.com/mwiki/index.php?title=Special%3ASearch&search=" "cpp[ref]")

;; QT
(straight-use-package 'qml-mode)

(provide 'modo-cpp)
;;; modo-cpp.el ends here

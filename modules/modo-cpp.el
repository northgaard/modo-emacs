;;; modo-cpp.el --- C++ programming -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for programming in C++ (work in progress)

;;; Code:

;; Clone from emacsmirror, since the default is to clone the entire
;; cmake repository (seems a bit excessive).
(straight-use-package '(cmake-mode :type git
                                   :host github
                                   :repo "emacsmirror/cmake-mode"))
(use-package cmake-mode)

(straight-use-package 'clang-format)
(use-package clang-format
  :commands (clang-format-region clang-format-buffer)
  :config
  (defun modo--clang-format-on-save ()
    (when (member major-mode '(c-mode c++-mode glsl-mode))
      (progn
        (when (locate-dominating-file "." ".clang-format")
          (clang-format-buffer))
          ;; Return nil, to continue saving
          nil)))
  (when (executable-find clang-format-executable)
    (add-hook 'before-save-hook #'modo--clang-format-on-save)))

(straight-use-package 'modern-cpp-font-lock)
(defun modo--load-clang-format ()
  (require 'clang-format))
(use-package c++-mode
  :ghook ('c++-mode-hook #'modo--load-clang-format nil nil t)
  :hook (c++-mode . modern-c++-font-lock-mode))

(provide 'modo-cpp)
;;; modo-cpp.el ends here

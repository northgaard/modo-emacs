;;; modo-rust.el --- Rust lang support -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for Rust programming

;;; Code:

(straight-use-package 'rust-mode)
(use-package rust-mode
  :config
  (setq rust-format-on-save t))

(provide 'modo-rust)
;;; modo-rust.el ends here

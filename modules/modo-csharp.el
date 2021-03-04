;;; modo-csharp.el --- C# programming -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for programming in C#.

;;; Code:

(straight-use-package 'csharp-mode)
(use-package csharp-mode
  :init
  (add-to-list 'projectile-other-file-alist '("xaml" "xaml.cs"))
  (add-to-list 'projectile-other-file-alist '("xaml.cs" "xaml"))
  (when modo-ligatures-active
    (ligature-set-ligatures 'csharp-mode '(".?" "??"))))

(provide 'modo-csharp)
;;; modo-csharp.el ends here

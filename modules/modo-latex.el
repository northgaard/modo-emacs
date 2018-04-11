;;; modo-latex.el --- working with LaTeX -*- lexical-binding: t -*-
;;; Commentary

;; Writing LaTeX supported by AUCTeX and RefTeX

;;; Code:

;;; AUCTeX + RefTeX
(if (not (eq system-type 'windows-nt))
    (progn
      (straight-use-package 'auctex)
      (use-package tex-site :demand t)

      (use-package tex
        :config
        (setq TeX-auto-save t)
        (setq TeX-parse-self t)
        (setq-default TeX-master nil))

      (use-package latex
        :config
        (setq LaTeX-fill-break-at-separators nil)
        (setq LaTeX-babel-hyphen nil)
        (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
        (add-hook 'LaTeX-mode-hook 'auctex-latexmk-setup))

      (use-package reftex
        :after latex
        :commands (turn-on-reftex)
        :general
        (:keymaps '(reftex-select-label-mode-map
                    reftex-select-bib-mode-map)
                  "j" 'reftex-select-next
                  "k" 'reftex-select-previous
                  "d" 'reftex-select-accept)
        (:keymaps 'reftex-toc-mode-map
                  "j" 'reftex-toc-next
                  "k" 'reftex-toc-previous
                  "d" 'reftex-toc-goto-line-and-hide)
        (modo-define-major-leader-key :keymaps 'LaTeX-mode-map
                                      "r" 'reftex-reference
                                      "c" 'reftex-citation
                                      "l" 'reftex-label
                                      "p" 'reftex-parse-all
                                      "t" 'reftex-toc)
        :config
        (setq reftex-plug-into-AUCTeX t)
        (setq reftex-toc-split-windows-horizontally t)
        (setq reftex-toc-split-windows-fraction 0.5))

;;; LatexMK
      (defvar modo-latex-build-command (if (executable-find "latexmk") "LatexMk" "LaTeX")
        "Default command for building LaTeX documents.")
      (defun modo-latex-build ()
        (interactive)
        (progn
          (let ((TeX-save-query nil))
            (TeX-save-document (TeX-master-file)))
          (TeX-command modo-latex-build-command 'TeX-master-file -1)))

      (straight-use-package 'auctex-latexmk)
      (use-package auctex-latexmk
        :after latex
        :commands (auctex-latexmk-setup)
        :general
        (modo-define-major-leader-key :keymaps 'LaTeX-mode-map
                                      "e" 'TeX-next-error
                                      "b" '(modo-latex-build
                                            :which-key "latex-build"))
        :config
        (setq auctex-latexmk-inherit-TeX-PDF-mode t)))
  (progn
    (straight-use-package-no-build 'auctex)
    (straight-use-package-no-build 'auctex-latexmk)))

(provide 'modo-latex)
;;; modo-latex.el ends here

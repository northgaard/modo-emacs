;; -*- lexical-binding: t -*-
(require 'modo-core (concat user-emacs-directory "core/modo-core"))

;;; org mode
(modo-add-package org "org-mode/lisp")
(modo-add-package org-contribdir "org-mode/contrib/lisp")
(modo-add-package-single org-bullets "org-bullets/org-bullets.el")
(use-package org
  :config
  (setq org-startup-indented t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-fontify-natively t)
  (use-package org-bullets
    :commands (org-bullets-mode)
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    (setq org-bullets-bullet-list '("•")))
  (use-package org-drill :demand t
    :init
    (require 'cl) ;; Needs the outdated cl lib
    (setq org-id-locations-file (expand-file-name "org-id-locations" modo-cache-dir))
    (setq org-drill-add-random-noise-to-intervals-p t)
    (setq org-drill-learn-fraction 0.4)))

(modo-add-package-single evil-org "evil-org-mode/evil-org.el")
(use-package evil-org
  :after org
  :commands (evil-org-mode)
  :init
  (add-hook 'org-mode-hook (lambda () (evil-org-mode 1)))
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional)))

;;; ivy/ivy hydra/counsel/swiper
;; Ivy needs a more precise recipe because all four packages are in one repo
(quelpa `(ivy :fetcher file
              :path ,(concat modo-repo-dir "swiper")
              :files (:defaults
                      (:exclude "swiper.el" "counsel.el" "ivy-hydra.el")
                      "doc/ivy-help.org")))
(add-to-list 'package-selected-packages 'ivy)
(modo-add-package-single ivy-hydra "swiper/ivy-hydra.el")
(modo-add-package-single swiper "swiper/swiper.el")
(modo-add-package-single counsel "swiper/counsel.el")
(use-package ivy :demand t
  :commands (ivy-switch-buffer ivy-resume)
  :diminish ivy-mode
  :general
  (modo-define-leader-key "r" 'ivy-resume)
  (:keymaps 'ivy-minibuffer-map
            "C-j" 'ivy-next-line
            "C-k" 'ivy-previous-line
            "C-h" 'ivy-beginning-of-buffer
            "C-l" 'ivy-end-of-buffer
            "M-j" 'ivy-next-history-element
            "M-k" 'ivy-previous-history-element)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (define-key ivy-mode-map [remap switch-to-buffer]
    #'ivy-switch-buffer)
  (use-package ivy-hydra
    :commands (hydra-ivy/body))
  (ivy-mode 1))

(use-package counsel
  :commands (counsel-apropos
             counsel-find-file
             counsel-recentf
             counsel-bookmark
             counsel-M-x
             counsel-describe-function
             counsel-describe-variable)
  :init
  (define-key ivy-mode-map [remap apropos] #'counsel-apropos)
  (define-key ivy-mode-map [remap find-file] #'counsel-find-file)
  (define-key ivy-mode-map [remap recentf-open-files]
    #'counsel-recentf)
  (define-key ivy-mode-map [remap bookmark-jump] #'counsel-bookmark)
  (define-key ivy-mode-map [remap execute-extended-command]
    #'counsel-M-x)
  (define-key ivy-mode-map [remap describe-function] #'counsel-describe-function)
  (define-key ivy-mode-map [remap describe-variable] #'counsel-describe-variable))

(use-package swiper
  :commands (swiper)
  :init
  (define-key ivy-mode-map [remap isearch-forward] 'swiper))

;;; elisp
(defun modo--elisp-extra-fontification ()
  "Fontify modo functions."
  (font-lock-add-keywords
   nil `(("\\(^\\|\\s-\\|,\\)(\\(\\(modo\\|\\+\\)[^) ]+\\)[) \n]" (2 font-lock-keyword-face)))))

(add-hook 'emacs-lisp-mode-hook #'modo--elisp-extra-fontification)

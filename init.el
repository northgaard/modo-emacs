;;; package.el
(setq package-enable-at-startup nil)
(setq package-user-dir (expand-file-name "build" user-emacs-directory))
;; (package-initialize) is called when quelpa is bootstrapped

;;; Initialize quelpa
(setq quelpa-self-upgrade-p nil)
;; Don't use MELPA
(setq quelpa-update-melpa-p nil)
(setq quelpa-checkout-melpa-p nil)
;; Bootstrap using local install
(setq quelpa-ci-dir (concat user-emacs-directory "packages/quelpa"))
(load (expand-file-name "bootstrap.el" quelpa-ci-dir))

;;; use-package
(quelpa `(diminish :fetcher file
		   :path ,(expand-file-name "diminish.el"
					    (concat user-emacs-directory
						    "packages/diminish"))
		   :version original))
(quelpa `(bind-key :fetcher file
		   :path ,(expand-file-name "bind-key.el"
					    (concat user-emacs-directory
						    "packages/use-package"))
		   :version original))
(quelpa `(use-package :fetcher file
		      :path ,(expand-file-name "use-package.el"
					       (concat user-emacs-directory
						       "packages/use-package"))))
(eval-when-compile
  (require 'use-package))
(require 'diminish)

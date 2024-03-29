;;; modo-dired.el --- file navigation with dired -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration and extension of the built-in dired file manager.

;;; Code:

(straight-use-package 'dired-subtree) ; We `require' it in the dired form below
(defun modo--subtree-expand-single-directory ()
  "When expanding a dired subtree, if the folder expanded
contains just a single non-empty folder, expand that folder as
well."
  (let* ((files (save-excursion
                  (dired-prev-dirline 1)
                  (seq-remove
                   (lambda (file)
                     (let ((non-full (file-name-nondirectory file)))
                       (or (string-equal "." non-full)
                           (string-equal ".." non-full))))
                   (directory-files (dired-get-filename) 'full))))
         (subdir (when (= (length files) 1)
                   (car files))))
    (when (and subdir
               (file-accessible-directory-p subdir)
               (not (directory-empty-p subdir)))
      (dired-subtree-insert))))

(use-package dired
  :general
  (modo-define-leader-key :keymaps 'override
    "d" 'dired-jump)
  :init
  (setq dired-auto-revert-buffer t
        dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Ask whether destination dirs should get created when copying/removing files.
        dired-create-destination-dirs 'ask
        ;; Where to store image caches
        image-dired-dir (concat modo-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        image-dired-thumb-size 150)
  :config
  (require 'dired-subtree)
  (add-hook 'dired-subtree-after-insert-hook #'modo--subtree-expand-single-directory)
  (evil-collection-require 'dired)
  (evil-collection-dired-setup)
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd modo-leader) nil))

(straight-use-package 'diredfl)
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired-x
  :hook (dired-mode . dired-omit-mode)
  :general
  (modo-define-major-leader-key :keymaps 'dired-mode-map
    "o" 'dired-omit-mode)
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store\\'"
                "\\|^.project\\(?:ile\\)?\\'"
                "\\|^.\\(svn\\|git\\)\\'"
                "\\|^.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  ;; Let OS decide how to open certain files
  (when-let (cmd (cond (IS-MAC "open")
                       (IS-LINUX "xdg-open")
                       (IS-WINDOWS "start")))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd)))))

(straight-use-package 'dired-git-info)
(use-package dired-git-info
  :general
  (modo-define-major-leader-key :keymaps 'dired-mode-map
    "g" 'dired-git-info-mode)
  :config
  (setq dgi-commit-message-format "%h %cs %s"
        dgi-auto-hide-details-p nil))

(provide 'modo-dired)
;;; modo-dired.el ends here

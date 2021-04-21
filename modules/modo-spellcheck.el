;;; modo-spellcheck.el --- the bulwark against pedants -*- lexical-binding: t -*-
;;; Commentary:

;; Check spelling using flyspell.

;;; Code:

(defcustom modo-spellchecker-program
  (if IS-WINDOWS
      "hunspell"
    "aspell")
  "Program used for spellchecking.")

(use-package flyspell
  :general
  (modo-define-leader-key
    :keymaps 'override
    "k" '(:ignore t :wk "spellcheck")
    "kb" 'modo-flyspell-buffer
    "kq" 'flyspell-clear-overlays
    "kt" 'modo-toggle-flyspell-mode
    "kl" 'flyspell-lazy-local-mode)
  :init
  (with-eval-after-load 'magit
    (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))
  (unless (getenv "LANG")
    (setenv "LANG" "en_US"))
  :config
  (setq ispell-program-name (executable-find modo-spellchecker-program)
        ispell-list-command "--list"
        ispell-extra-args '("--sug-mode=ultra")
        flyspell-issue-message-flag nil)
  (defun flyspell-prog-buffer ()
    "Spellcheck buffer as in `flyspell-prog-mode'."
    (interactive)
    (let ((flyspell-generic-check-word-predicate
           #'flyspell-generic-progmode-verify))
      (flyspell-buffer)))
  (defun flyspell-clear-overlays ()
    "Clear flyspell overlays.

If the region is active, clear overlays in region. Otherwise
clear overlays in entire buffer."
    (interactive)
    (pcase-let ((`(,beg . ,end)
                 (if (region-active-p)
                     (cons (region-beginning) (region-end))
                   (cons (point-min) (point-max)))))
      (flyspell-delete-region-overlays beg end)))
  (defun modo-flyspell-buffer ()
    "Spellcheck the current buffer, respecting major mode.

This will spellcheck comments and strings in `prog-mode' buffers,
and check everything anywhere else."
    (interactive)
    (if (derived-mode-p 'prog-mode)
        (flyspell-prog-buffer)
      (flyspell-buffer)))
  (defun modo-toggle-flyspell-mode ()
    "Toggle `flyspell-mode', respecting major mode.

This will toggle `flyspell-prog-mode' in `prog-mode' buffers, and
`flyspell-mode' everywhere else."
    (interactive)
    (if (and (derived-mode-p 'prog-mode)
             (not flyspell-mode))
        (flyspell-prog-mode)
      (flyspell-mode 'toggle))))

(straight-use-package 'flyspell-lazy)
(use-package flyspell-lazy
  :config
  (setq flyspell-lazy-idle-seconds 1.5
        flyspell-lazy-window-idle-seconds 5))

(define-minor-mode flyspell-lazy-local-mode
  "Turn on flyspell-lazy-local-mode.

This is more or less a local equivalent of the global
`flyspell-lazy-mode', but is local and automatically handles
turning on `flyspell-mode' as well. Using a local minor mode
allows for greater granularity in controlling spell checking. The
base `flyspell-mode' can make typing sluggish for larger buffers,
and so you would use this mode to prevent it. However, for some
buffers (for instance magit commit buffers), it's advantageous to
get the instant feedback of the base `flyspell-mode', and the
decrease in performance is worth it. Using the flyspell-lazy
package like this is not possible with the global
`flyspell-lazy-mode'."
  :group 'flyspell-lazy
  (progn
    (require 'flyspell-lazy)
    (cond
     (flyspell-lazy-local-mode
      (if (derived-mode-p 'prog-mode)
          (flyspell-prog-mode)
        (flyspell-mode 1))
      (flyspell-lazy-load))
     (t
      (flyspell-lazy-unload)
      (flyspell-mode -1)))))

(straight-use-package 'flyspell-correct)
(use-package flyspell-correct
  :general
  (modo-define-leader-key
    :keymaps 'override
    "kc" 'flyspell-correct-wrapper
    "kk" 'flyspell-correct-wrapper
    "ks" 'modo-flyspell-correct-session)
  :config
  (with-eval-after-load 'embark
    (defun flyspell-correct-embark-skip (word)
      (throw 'flyspell-correct-return (cons 'skip word)))
    (defun flyspell-correct-embark-save (word)
      (throw 'flyspell-correct-return (cons 'save word)))
    (defun flyspell-correct-embark-session (word)
      (throw 'flyspell-correct-return (cons 'session word)))
    (defun flyspell-correct-embark-buffer (word)
      (throw 'flyspell-correct-return (cons 'buffer word)))
    (defun flyspell-correct-embark-stop (word)
      (throw 'flyspell-correct-return (cons 'stop word)))
    (defvar embark-flyspell-correct-map (make-sparse-keymap)
      "Keymap for actions with flyspell-correct.")
    (general-define-key :keymaps 'embark-flyspell-correct-map
                        "s" '(flyspell-correct-embark-save :wk "save")
                        "S" '(flyspell-correct-embark-session :wk "save (session)")
                        "b" '(flyspell-correct-embark-buffer :wk "save (buffer)")
                        "k" '(flyspell-correct-embark-skip :wk "skip")
                        "q" '(flyspell-correct-embark-stop :wk "stop"))
    (add-to-list 'embark-keymap-alist '(flyspell-correct . embark-flyspell-correct-map))
  (defun flyspell-correct-embark (candidates word)
    (catch 'flyspell-correct-return
      ;; Using cl-letf here doesn't feel right, probably should open
      ;; an issue on the embark repo.
      (cl-letf (((symbol-function 'embark--quit-and-run)
                 #'funcall))
        (completing-read
         (format "Correcting '%s': " word)
         (lambda (input predicate action)
           (if (eq action 'metadata)
               '(metadata (display-sort-function . identity)
                          (cycle-sort-function . identity)
                          (category . flyspell-correct))
             (complete-with-action action candidates input predicate)))))))
  (setq flyspell-correct-interface #'flyspell-correct-embark))
  (defun modo-flyspell-correct-session ()
    "Start an interactive session to correct spelling mistakes."
    (interactive)
    (let ((flyspell-was-active flyspell-mode))
      (unwind-protect
          (progn
            (modo-flyspell-buffer)
            (flyspell-correct-move (point-min) 'forward 'rapid))
        (unless flyspell-was-active
          (flyspell-delete-all-overlays))))))

(provide 'modo-spellcheck)
;;; modo-spellcheck.el ends here

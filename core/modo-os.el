;;; modo-os.el --- os specific configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Core settings that are specific to a certain operating system go here.

;;; Code:

;;; Windows

;; Since we are using straight.el, git is required to be installed. Thus,
;; M-x shell is configured to use git-bash once the user has configured
;; its location. DO NOT expect miracles with this, but its something.
(when IS-WINDOWS

  (defcustom modo-git-path nil
    "String specifying the root directory of the git for windows installation."
    :type 'directory
    :group 'modo-emacs)

  (setq w32-pipe-buffer-size 65536 ; 64 KB
        w32-pipe-read-delay 0
        w32-get-true-file-attributes nil)

  ;; TODO: Validate path
  (when (stringp modo-git-path)
    (setq explicit-shell-file-name
          (expand-file-name "usr/bin/bash.exe" modo-git-path)
          shell-file-name explicit-shell-file-name
          explicit-bash.exe-args '("--login" "-i"))
    (setenv "SHELL" shell-file-name)
    (add-to-list 'exec-path
                 (concat (file-name-as-directory modo-git-path) "bin/"))
    (add-to-list 'exec-path
                 (concat (file-name-as-directory modo-git-path) "usr/bin/"))
    ;; Set find to be the one from git bash, which should now be at
    ;; the front of PATH. Note that this may or may not cause issues
    ;; if the path contains spaces.
    (setq find-program (executable-find "find"))
    (setq straight-find-executable find-program)
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

;;; WSL
(when IS-WSL
  ;; Open links in your default browser
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program cmd-exe
            browse-url-generic-args cmd-args
            browse-url-browser-function 'browse-url-generic))))

;;; Linux
(when IS-LINUX
  (setq select-enable-clipboard t
        select-enable-primary t
        ;; Maybe help out with "Timed out waiting for reply from
        ;; selection owner" issue.
        ;; See Emacs bug #16737
        ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2015-01/msg00529.html
        x-selection-timeout 50)

  (defun modo-insert-primary ()
    "Insert the primary selection at point."
    (interactive)
    (insert (gui-get-primary-selection))))

(provide 'modo-os)
;;; modo-os.el ends here

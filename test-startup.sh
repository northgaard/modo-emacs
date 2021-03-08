#!/bin/sh -e
echo "Attempting startup..."
${EMACS:=emacs} -nw --batch \
                --eval '(let ((debug-on-error t)
                              (user-init-file (expand-file-name "init.el"))
                              (load-path (delq default-directory load-path)))
                           (custom-set-variables (quote (url-show-status nil))
                                                 (quote (user-emacs-directory default-directory))
                                                 (quote (straight-vc-git-default-clone-depth 1)))
                           (load-file user-init-file)
                           (run-hooks (quote after-init-hook)))'
echo "Startup successful"

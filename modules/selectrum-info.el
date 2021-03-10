;;; selectrum-info.el --- Complete info nodes -*- lexical-binding: t -*-
;;; Commentary:

;; Browse info nodes using selectrum based completion.

;;; Code:

(defvar Info-directory-list)
(defvar Info-additional-directory-list)
(defvar Info-default-directory-list)
(declare-function info-initialize "info")
(declare-function cl-mapcar "cl-lib")

(defvar selectrum-info-history nil
  "Completion history for `selectrum-info' and derived commands.")

(defcustom selectrum-info-default-other-window t
  "Whether `selectrum-info' (and derived commands) should display
the Info buffer in the other window by default. Use a prefix argument to
do the opposite."
  :type 'boolean
  :group 'selectrum)

(defun selectrum--info-get-child-node (top-node)
  "Create and select from a list of Info nodes found in the parent node TOP-NODE."
  (let (;; It's reasonable to assume that sections are intentionally
        ;; ordered in a certain way, so we preserve that order.
        (selectrum-should-sort nil)
        ;; Headers look like "* Some Thing::      Description",
        ;; where descriptions are optional and might continue on
        ;; the next line.
        (sub-topic-format (rx "* "
                              (group (+? (not ?:)))
                              "::"
                              ;; Include the description, if one exists.
                              ;; If it doesn't, the line ends immediately.
                              (or "\n"
                                  (seq
                                   (0+ blank)
                                   (group (+? anychar))
                                   ;; Sometimes a heading follows on the next line,
                                   ;; and sometimes there's any empty blank line
                                   ;; (such as before a section title).  For now,
                                   ;; assume continuation lines use indentation and
                                   ;; other lines don't.
                                   ;; (or "\n\n" "\n*")
                                   "\n" (not blank))))))
    (let ((candidates
           (save-selected-window
             (save-match-data
               (with-temp-buffer
                 ;; Some nodes created from multiple files.
                 (info top-node (current-buffer))
                 (goto-char (point-min))
                 (cl-loop while (re-search-forward sub-topic-format nil t)
                       do (forward-line 0)         ; Go back to start of line.
                       collect (match-string 1) into node-names
                       collect (match-string 2) into descriptions
                       ;; If a node has a description, it helps if that description is
                       ;; also searchable.  For the normal ‘*Completions*’ buffer, that
                       ;; can be done using regular annotation data.  For Selectrum,
                       ;; (in which annotations can't currently be matched against),
                       ;; this can be done by including the annotation in the
                       ;; displayed text and setting the ‘selectrum-candidate-full’
                       ;; property to be the actual node name.
                       finally return
                       (cl-mapcar (lambda (node-name description)
                                    (cons
                                     (concat
                                      node-name
                                      (when description
                                        (propertize
                                         (concat " - "
                                                 (replace-regexp-in-string
                                                  "\n" ""
                                                  (replace-regexp-in-string
                                                   " +" " " description)))
                                         'face 'completions-annotations)))
                                     node-name))
                                  node-names descriptions)))))))

      (cdr (assoc (completing-read
                   "Info Sub-Topic: "
                   candidates
                   nil t nil 'selectrum-info-history)
                  candidates)))))

;;;###autoload
(defun selectrum-info (other-window-opposite-p &optional top-node)
  "Go to a node of an Info topic.
With a prefix argument, do the opposite
of `selectrum-info-default-other-window'.
For example, you can go to \"(magit)Notes\" by selecting \"magit\", then \"Notes\" ."
  (interactive "P")

  ;; Initialize Info information so that the proper directories
  ;; can be found.
  (info-initialize)

  (save-match-data
    (let* ((use-other-window (if other-window-opposite-p
                                 (not selectrum-info-default-other-window)
                               selectrum-info-default-other-window))
           ;; Get all Info files.
           (node-files
            (cl-loop for directory in (append (or Info-directory-list
                                                  Info-default-directory-list)
                                              Info-additional-directory-list)
                  ;; If the directory exists
                  when (file-directory-p directory)
                  ;; get all files with ".info" in their name.
                  append (directory-files directory nil "\\.info" t)))

           ;; Get the names of the Info nodes, based on the file names.
           (node-names (cl-remove-duplicates
                        (cl-loop for file in node-files
                              do (string-match "\\(.+?\\)\\." file)
                              collect (match-string 1 file))
                        :test #'equal))

           ;; Select a top node/topic.
           (chosen-top-node (cond
                              ((null top-node)
                               (completing-read "Info Topic: " node-names nil t))
                              ((member top-node node-names)
                               top-node)
                              (t (error "Top-level Info node does not exist: %s"
                                        top-node))))

           ;; Select a child node.
           (chosen-child-node (selectrum--info-get-child-node chosen-top-node)))

      ;; Go to the chosen child node.
      (funcall (if use-other-window
                   #'info-other-window
                 #'info)
               (format "(%s)%s" chosen-top-node chosen-child-node)))))

;;;###autoload
(defun selectrum-info-elisp-manual (other-window-opposite-p)
  "Like `selectrum-info', but directly choose nodes from the Emacs Lisp (Elisp) manual."
  (interactive "P")
  (selectrum-info other-window-opposite-p "elisp"))

;;;###autoload
(defun selectrum-info-emacs-manual (other-window-opposite-p)
  "Like `selectrum-info', but directly choose nodes from the Emacs manual."
  (interactive "P")
  (selectrum-info other-window-opposite-p "emacs"))

;;;###autoload
(defun selectrum-info-org-manual (other-window-opposite-p)
  "Like `selectrum-info', but directly choose nodes from the Org manual."
  (interactive "P")
  (selectrum-info other-window-opposite-p "org"))

(provide 'selectrum-info)
;;; selectrum-info.el ends here

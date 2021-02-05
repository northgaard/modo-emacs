;;; modo-lib.el --- library functions -*- lexical-binding: t -*-
;;; Commentary:

;; Useful helper functions and macros.

;;; Code:

(defun modo--is-elisp-file-p (file)
  (string-equal (file-name-extension file) "el"))

(defun modo--extract-name (string)
  (declare (pure t) (side-effect-free t))
  (let* ((str (file-name-sans-extension string))
         (split (split-string str "-")))
    (string-join (cdr split) "-")))

(defun modo--get-features (dir)
  (let ((features nil)
        (files (directory-files dir nil "modo")))
    (dolist (file files)
      (when (modo--is-elisp-file-p file)
        (push (modo--extract-name file) features)))
    (nreverse features)))

(defun modo-font-family-exists-p (family-name)
  "Checks if the font family FAMILY-NAME exists. Returns the font-entity
if it does, nil otherwise."
  (find-font (font-spec :family family-name)))

(defun modo--get-subdirs (path)
  (mapcar #'file-name-nondirectory (f-directories path)))

(defun modo--map-symbol (mapping symbol)
  (intern (funcall mapping (symbol-name symbol))))

(defun modo-pluralize (count singular &optional plural)
  "Returns the singular or plural form of a word, depending on
  the value of COUNT. If only the form SINGULAR is provided, it
  is assumed that the word has a regular plural. Otherwise, the
  input PLURAL can be used to provide an irregular plural."
  (declare (pure t) (side-effect-free t))
  (cond
   ((eq count 1)
    singular)
   ((> count 1)
    (if plural
        plural
      (concat singular "s")))
   (t
    (error "The input count must be a natural number"))))

(defmacro modo-quieten (&rest body)
  "Runs the body with calls to `message' suppressed."
  `(cl-letf (((symbol-function 'message) #'ignore))
     ,@body))

;; TODO: Add docstring and proper docstring for both backing value and
;; initializer function
(defmacro modo-deflazy (name evaluator &optional docstring)
  (declare (doc-string 3))
  (let ((initializer (cond
                      ((functionp evaluator)
                       `(funcall ,evaluator))
                      ((listp evaluator)
                       evaluator)
                      (t
                       (error "Malformed evaluator"))))
        (initializer-name (modo--map-symbol (lambda (str)
                                              (concat str "-value"))
                                              name)))
    `(progn
       (defvar ,name nil ,docstring)
       (defun ,initializer-name ()
           (unless ,name
             (setq ,name ,initializer))
         ,name))))

(cl-defmacro modo-add-hook ((hook &key name transient) &body body)
  "Run BODY in HOOK.

If the optional argument NAME is specified, use that to name the
generated defun. If the optional argument TRANSIENT is specified,
the hook function removes itself from HOOK when run."
  (declare (indent 1))
  (let* ((funcname (intern (if name
                              name
                             (format "modo-hook--%s" hook))))
         (defun-form `(defun ,funcname () ,@body)))
    (when transient
      (add-to-list 'defun-form `(remove-hook ',hook ',funcname) t))
    `(progn
       ,defun-form
       (add-hook ',hook ',funcname))))

(defun modo-url-search (query-url prompt)
  "Search the web for PROMPT with a search url constructed with QUERY-URL."
  (browse-url
   (concat query-url (url-hexify-string prompt))))

(provide 'modo-lib)
;;; modo-lib.el ends here

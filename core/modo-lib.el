;;; modo-lib.el --- library functions -*- lexical-binding: t -*-
;;; Commentary:

;; Useful helper functions and macros.

;;; Code:

(defun modo--extract-name (string)
  (declare (pure t) (side-effect-free t))
  (let* ((str (file-name-sans-extension string))
         (split (split-string str "-")))
    (string-join (cdr split) "-")))

(defun modo--get-features (dir)
  (let ((features nil)
        (files (directory-files dir nil "modo")))
    (dolist (file files)
      (push (modo--extract-name file) features))
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

(provide 'modo-lib)
;;; modo-lib.el ends here

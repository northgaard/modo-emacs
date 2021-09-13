;;; early-init.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)
(fset #'package--ensure-init-file #'ignore) ; DEPRECATED: removed in Emacs 28

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)

(defvar modo--early-init-loaded t)

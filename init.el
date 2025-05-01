;;; init.el --- Bootstrap further configurations -*- lexical-binding: nil; -*-


;; this _does not_ recursively list directories
(add-to-list 'load-path (directory-file-name (locate-user-emacs-file "lisp")))

(require 'config)

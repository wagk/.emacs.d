;;; init.el --- Bootstrap further configurations

;; this _does not_ recursively list directories
(add-to-list 'load-path (directory-file-name (locate-user-emacs-file "lisp")))

(require 'config)

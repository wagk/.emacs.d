(defcustom my-snippet-override-mode-list '(groovy-mode
                                           emacs-lisp-mode)
  "The list of modes that have overriding snippets for DEBUG, FIXME,
TODO, NOTE.")

(defun my-snippet-overrided? ()
  (require 'dash)
  (-any-p (lambda (mode) (eq mode major-mode))
          my-snippet-override-mode-list))

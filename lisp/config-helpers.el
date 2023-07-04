;;; config-helpers -- Weird one-off helper functions

;;; Commentary:

;;; Code:

(cl-defun --thing-at-point-or-user-input ()
  "Prompt the user for input, defaulting to symbol at point if none."
  (require 'thingatpt)
  (let* ((thing (thing-at-point 'symbol))
         (prompt (concat "Grep for"
                         (when thing
                           (format " (default: %s)" thing))
                         ": "))
         (string (read-string prompt)))
    (if (string-empty-p string)
        thing
      string)))

(provide 'config-helpers)

;;; config-helpers.el ends here

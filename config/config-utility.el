;;; config/utility --- utility functions/macros

;;; Commentary:
;; Functions here should *not* include use-package or anything like that

;;; Code:

;;datetime things
;;;###autoload
(defvar current-date-time-format "%Y-%m-%dT%H:%M:%S"
  "Format of date to insert with `insert-current-date-time' func.
See help of `format-time-string' for possible replacements")

;;;###autoload
(defun insert-current-date-time ()
  "Insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-time-format (current-time))))

(provide 'config/utility)
;;; config/utility.el ends here

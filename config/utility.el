;;; utility.el --- utility functions/macros

;;; Commentary:
;; Functions here should *not* include use-package or anything like that

;;; Code:

;;datetime things
(defvar current-date-time-format "%Y-%m-%dT%H:%M:%S"
  "Format of date to insert with `insert-current-date-time' func.
See help of `format-time-string' for possible replacements")

(defun insert-current-date-time ()
  "Insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-time-format (current-time))))

(defun find-user-init-file ()
  "Edit `user-init-file` without opening a new window."
  (interactive)
  (find-file user-init-file)
  )


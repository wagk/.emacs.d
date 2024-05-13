;;; markdown-datetree.el --- Datetrees. In markdown.  -*- lexical-binding: t; -*-
;;

;; Author: Pang Tun Jiang <mail@pangt.dev>
;; Keywords: docs, tools,

;; We're going to hardcode the levels for the datetree at the moment:
;;
;; # Datetree (top-level heading)
;; ## 2024
;; ### 2024-05
;; #### 2024-05-07
;; ##### .... <title>

(require 'rx)

;; TODO (pangt): Assert that the hierarchy of year -> month -> day is correct
;; TODO (pangt): Provide helper functions to find, insert, and possibly fix
;; datetrees.
;; TODO (pangt): Ensure that insertion is in sorted order. Currently the
;; expectation is that we won't insert explicit dates (and only the current
;; date, which is constantly increasing) and so the insertions will always be
;; ordered. But that might not be true in the future and it will be good to
;; consider that in the design anyway.

(defgroup markdown-datetree nil
  "Functions for organizing a datetree within a markdown document.")

(defconst markdown-datetree-root-heading-regexp "# Datetree")

(defconst markdown-datetree-year-heading-regexp
  (rx "## " (group-n 1 (= 4 digit)) eol))

(cl-defun markdown-datetree-specific-year-heading-regexp (year)
  (cl-assert year)
  (cl-assert (stringp year))
  (rx "## " (literal year) eol))

(defconst markdown-datetree-month-heading-regexp
  (rx "### " (= 4 digit) "-" (in "0-1") digit eol))

(cl-defun markdown-datetree-specific-month-heading-regexp (year month)
  (cl-assert year)
  (cl-assert (stringp year))
  (cl-assert month)
  (cl-assert (stringp month))
  (rx "### " (literal year) "-" (literal month) eol))

(defconst markdown-datetree-day-heading-regexp
  (rx "#### " (= 4 digit) "-" (in "0-1") digit "-" (in "0-3") digit eol))

(cl-defun markdown-datetree-specific-day-heading-regexp (day)
  (cl-assert day)
  (cl-assert (stringp day))
  (rx "#### " (= 4 digit) "-" (in "0-1") digit "-" (in "0-3") digit eol))

(cl-defun markdown-datetree-specific-date-heading-regexp (year month day)
  (cl-assert year)
  (cl-assert (stringp year))
  (cl-assert month)
  (cl-assert (stringp month))
  (cl-assert day)
  (cl-assert (stringp day))
  (rx "#### " (literal year) "-" (literal month) "-" (literal day) eol))

(cl-defun markdown-datetree-go-to-datetree ()
  "In the buffer, find or create a level one heading 'Datetree'.
Moves the point to the end of the datetree root header."
  (goto-char (point-min))
  (unless (re-search-forward markdown-datetree-root-heading-regexp nil t)
    (goto-char (point-max))
    (insert "# Datetree"))
  (end-of-line))

(cl-defun markdown-datetree-go-to-year (&optional year)
  "In the buffer, find or create the year header.
Moves the point to the end of the year header."
  (require 'outline)
  (markdown-datetree-go-to-datetree)
  (let ((year (if year (number-to-string year)
                (format-time-string "%Y"))))
    (unless (-> year
                (markdown-datetree-specific-year-heading-regexp)
                (re-search-forward nil t))
      (outline-end-of-subtree)
      (newline)
      (insert (concat "## " year)))))

(cl-defun markdown-datetree-go-to-month (&optional year month)
  "In the buffer, find or create the month header.
Moves the point to the end of the month header."
  (require 'outline)
  (markdown-datetree-go-to-year year)
  (let ((month (if month (format "%02d" month)
                (format-time-string "%m")))
        (year (if year (number-to-string year)
                (format-time-string "%Y"))))
    (unless (-> (markdown-datetree-specific-month-heading-regexp year month)
                (re-search-forward nil t))
      (outline-end-of-subtree)
      (newline)
      (insert (concat "### " year "-" month)))))

(cl-defun markdown-datetree-go-to-day (&optional year month day)
  "In the buffer, find or create the day header.
Moves the point to the end of the day header."
  (require 'outline)
  (markdown-datetree-go-to-month year month)
  (let ((day (if day (format "%02d" day)
               (format-time-string "%d")))
        (month (if month (format "%02d" month)
                 (format-time-string "%m")))
        (year (if year (number-to-string year)
                (format-time-string "%Y"))))
    (unless (-> (markdown-datetree-specific-date-heading-regexp year month day)
                (re-search-forward nil t))
      (outline-end-of-subtree)
      (newline)
      (insert (concat "#### " year "-" month "-" day)))))

(provide 'markdown-datetree)

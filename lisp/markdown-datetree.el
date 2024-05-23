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
;; ##### 10:34 <title>

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
  (rx bol "## " (group-n 1 (= 4 digit))))

(cl-defun markdown-datetree-specific-year-heading-regexp (year)
  (cl-assert year)
  (cl-assert (stringp year))
  (rx bol "## " (literal year)))

(defconst markdown-datetree-month-heading-regexp
  (rx bol "### " (= 4 digit) "-" (in "0-1") digit))

(cl-defun markdown-datetree-specific-month-heading-regexp (year month)
  (cl-assert year)
  (cl-assert (stringp year))
  (cl-assert month)
  (cl-assert (stringp month))
  (rx bol "### " (literal year) "-" (literal month)))

(defconst markdown-datetree-day-heading-regexp
  (rx bol "#### " (= 4 digit) "-" (in "0-1") digit "-" (in "0-3") digit))

(cl-defun markdown-datetree-specific-day-heading-regexp (year month day)
  (cl-assert year)
  (cl-assert (stringp year))
  (cl-assert month)
  (cl-assert (stringp month))
  (cl-assert day)
  (cl-assert (stringp day))
  (rx bol "#### " (literal year) "-" (literal month) "-" (literal day)))

(defconst markdown-datetree-time-heading-regexp
  (rx bol "##### " (in "0-2") digit ":" (in "0-5") digit))

(cl-defun markdown-datetree-specific-time-heading-regexp (hour minute)
  (cl-assert hour)
  (cl-assert (stringp hour))
  (cl-assert minute)
  (cl-assert (stringp minute))
  (rx bol "##### " (literal hour) ":" (literal minute)))

(cl-defun markdown-datetree-find-instant ()
  "Finds a datetree to the most specific root that currently exists in the file."
  (interactive)
  (let* ((root markdown-datetree-root-heading-regexp)
         (year (format-time-string "%Y"))
         (year-rx (markdown-datetree-specific-year-heading-regexp year))
         (month (format-time-string "%m"))
         (month-rx (markdown-datetree-specific-month-heading-regexp year month))
         (day (format-time-string "%d"))
         (day-rx (markdown-datetree-specific-day-heading-regexp year month day))
         (hour (format-time-string "%H"))
         (minute (format-time-string "%M"))
         (time-rx (markdown-datetree-specific-time-heading-regexp hour minute)))
    (cl-find-if-not #'(lambda (rx) (re-search-forward rx nil :noerror))
                    (list root year-rx month-rx day-rx time-rx))
    (point)))

(cl-defun markdown-datetree-find-datetree-root ()
  (interactive)
  (goto-char (point-min))
  (prog1 (re-search-forward markdown-datetree-root-heading-regexp nil :noerror)
    (end-of-line)))

(cl-defun markdown-datetree-find-datetree-year (&optional year)
  (interactive)
  (when (markdown-datetree-find-datetree-root)
    (let* ((year (or (number-to-string year) (format-time-string "%Y")))
           (regex (markdown-datetree-specific-year-heading-regexp year)))
      (prog1 (re-search-forward regex nil :error)
        (end-of-line)))))

(cl-defun markdown-datetree-find-datetree-month (&optional year month)
  (interactive)
  (when (markdown-datetree-find-datetree-year year)
    (let* ((year (if year (number-to-string year) (format-time-string "%Y")))
           (month (if month (format "%02d" month) (format-time-string "%m")))
           (regex (markdown-datetree-specific-month-heading-regexp year month)))
      (prog1 (re-search-forward regex nil :noerror)
        (end-of-line)))))

(cl-defun markdown-datetree-find-datetree-day (&optional year month day)
  (interactive)
  (when (markdown-datetree-find-datetree-month year month)
    (let* ((year (if year (number-to-string year) (format-time-string "%Y")))
           (month (if month (format "%02d" month) (format-time-string "%m")))
           (day (if day (format "%02d" day) (format-time-string "%d")))
           (regex (markdown-datetree-specific-day-heading-regexp year month day)))
      (prog1 (re-search-forward regex nil :noerror)
        (end-of-line)))))

(cl-defun markdown-datetree-find-datetree-time (&optional hour minute)
  (interactive)
  (when (markdown-datetree-find-datetree-day)
    (let* ((hour (if hour (format "%02d" hour) (format-time-string "%H")))
           (minute (if minute (format "%02d" minute) (format-time-string "%M")))
           (regex (markdown-datetree-specific-time-heading-regexp hour minute)))
      (prog1 (re-search-forward regex nil :noerror)
        (end-of-line)))))

(cl-defun markdown-datetree-template-heading ()
  "When creating an org capture template, Determine which levels of headings are
  missing and return the appropriate string."
  (let ((heading ""))
    (save-excursion
      (unless (markdown-datetree-find-datetree-root)
        (setq heading (concat heading "# Datetree\n")))
      (unless (markdown-datetree-find-datetree-year)
        (setq heading (concat heading (format-time-string "## %Y\n"))))
      (unless (markdown-datetree-find-datetree-month)
        (setq heading (concat heading (format-time-string "### %Y-%m\n"))))
      (unless (markdown-datetree-find-datetree-day)
        (setq heading (concat heading (format-time-string "#### %F\n"))))
      (unless (markdown-datetree-find-datetree-time)
        (setq heading (concat heading (format-time-string "##### %H:%M")))))
    heading))

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
    (unless (-> (markdown-datetree-specific-day-heading-regexp year month day)
                (re-search-forward nil t))
      (outline-end-of-subtree)
      (newline)
      (insert (concat "#### " year "-" month "-" day)))))

(provide 'markdown-datetree)

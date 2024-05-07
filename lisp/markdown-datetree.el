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

;; TODO (pangt): Parse headers for date
;; TODO (pangt): Assert that the hierarchy of year -> month -> day is correct
;; TODO (pangt): Provide helper functions to find, insert, and possibly fix
;; datetrees.

(defgroup markdown-datetree nil
  "Functions for organizing a datetree within a markdown document.")

(defconst markdown-datetree-year-heading-regexp
  (rx (1+ ?#) " " (= 4 digit) eol))

(defconst markdown-datetree-month-heading-regexp
  (rx (1+ ?#) " " (= 4 digit) "-" (in "0-1") digit eol))

(defconst markdown-datetree-day-heading-regexp
  (rx (1+ ?#) " " (= 4 digit) "-" (in "0-1") digit "-" (in "0-3") digit eol))

(cl-defun markdown-datetree-go-to-date (&optional &key no-create)
  "Within the buffer, tries to find the heading for today's date.")

(cl-defun markdown-datetree--buffer-collect-headings (buffer)
  "Given BUFFER, collect all headings.
Collect:
- The point
- The level
- The text"
  (with-current-buffer buffer
    (let (points)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp config-markdown-header-regexp nil t)
          (setq points (append points
                               `((:point ,(line-beginning-position)
                                  :level ,(length (match-string 1))
                                  ;; might not be a date!
                                  :header ,(match-string 2)))))))
      points)))

(cl-defun markdown-datetree-p (buffer)
  (let ((headings (markdown-datetree--buffer-collect-headings buffer)))))

(provide 'markdown-datetree)

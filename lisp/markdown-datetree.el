;;; markdown-datetree.el --- Datetrees. In markdown.  -*- lexical-binding: t; -*-

;; Author: Pang Tun Jiang <mail@pangt.dev>
;; Keywords: docs, tools,

(require 'markdown-mode)

;; TODO (pangt): Parse headers for date
;; TODO (pangt): Assert that the hierarchy of year -> month -> day is correct
;; TODO (pangt): Provide helper functions to find, insert, and possibly fix
;; datetrees.

(defconst markdown-datetree-header-regexp
  (rx bol
      (group-n 1 (one-or-more "#"))
      (one-or-more whitespace)
      (group-n 2 (one-or-more any))
      (zero-or-more whitespace)
      eol)
  "Matches the markdown header I use (the ones that start with \"#\").
Capture groups:
- 1: The headings. The length of this is the depth of the header.
- 2: The contents of the header.

Accounts for trailing whitespace.")

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
        (while (search-forward-regexp markdown-datetree-header-regexp nil t)
          (setq points (append points
                               `((:point ,(line-beginning-position)
                                  :level ,(length (match-string 1))
                                  ;; might not be a date!
                                  :header ,(match-string 2)))))))
      points)))

(cl-defun markdown-datetree-p (buffer)
  (let ((headings (markdown-datetree--buffer-collect-headings buffer)))))

(provide 'markdown-datetree)

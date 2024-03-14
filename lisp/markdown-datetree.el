;;; markdown-datetree.el --- Datetrees. In markdown.  -*- lexical-binding: t; -*-

;; Author: Pang Tun Jiang <mail@pangt.dev>
;; Keywords: docs, tools,

(require 'buttercup)
(require 'markdown-mode)

;; TODO (pangt): Parse headers for date
;; TODO (pangt): Assert that the hierarchy of year -> month -> day is correct
;; TODO (pangt): Provide helper functions to find, insert, and possibly fix
;; datetrees.

(defconst markdown-datetree-header-regexp
  (rx bol
      (one-or-more "#")
      (one-or-more whitespace)
      (group-n 1 (one-or-more any))
      eol)
  "Matches the markdown header I use (the ones that start with \"#\"). Captures
  as first group the contents of the header, so we can check the date.")

(cl-defun markdown-datetree-parse-buffer (buffer)
  "Given BUFFER, parse it as a datetree.
Return a list of datetree elements."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let (points)
        (while (search-forward-regexp markdown-datetree-header-regexp nil t))
        (push (line-beginning-position) points)))))

(cl-defun markdown-datetree-parse-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (markdown-datetree-parse-buffer (current-buffer))))

(provide 'markdown-datetree)

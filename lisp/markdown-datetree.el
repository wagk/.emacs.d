;;; markdown-datetree.el --- Datetrees. In markdown.  -*- lexical-binding: t; -*-

;; Author: Pang Tun Jiang <mail@pangt.dev>
;; Keywords: docs, tools,

(require 'buttercup)
(require 'markdown-mode)

;; TODO (pangt): Parse headers for date

(defconst markdown-datetree-header-regexp
  (rx bol
      (one-or-more "#")
      (one-or-more whitespace)
      (group-n 1 (one-or-more any))
      eol)
  "Matches the markdown header I use (the ones that start with \"#\"). Captures
  as first group the contents of the header, so we can check the date.")

(cl-defun markdown-datetree-parse-file (filename)
  "Given FILENAME, parse it as a datetree.
Return a list of datetree elements."
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (let (points)
      (while (search-forward-regexp markdown-datetree-header-regexp nil t)
        (push (line-beginning-position) points)))))

(provide 'markdown-datetree)

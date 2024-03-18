;;; markdown-datetree.el --- Datetrees. In markdown.  -*- lexical-binding: t; -*-

;; Author: Pang Tun Jiang <mail@pangt.dev>
;; Keywords: docs, tools,

(require 'markdown-mode)

;; TODO (pangt): Parse headers for date
;; TODO (pangt): Assert that the hierarchy of year -> month -> day is correct
;; TODO (pangt): Provide helper functions to find, insert, and possibly fix
;; datetrees.

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

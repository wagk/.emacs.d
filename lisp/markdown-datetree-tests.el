;;; markdown-datetree-tests.el --- markdown-datetree test cases  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Pang Tun Jiang

;; Author: Pang Tun Jiang <mail@pangt.dev>
;; Keywords: docs, tools

(require 'markdown-datetree)

(ert-deftest find-datetree-root-empty-buffer ()
  (with-temp-buffer
    (should-not (markdown-datetree-find-datetree-root))))

(ert-deftest find-datetree-root-non-empty-buffer ()
  (with-temp-buffer
    (insert "# Datetree")
    (should (markdown-datetree-find-datetree-root))))

(provide 'markdown-datetree-tests)

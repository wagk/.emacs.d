(require 'use-package)

;; https://github.com/magnars/dash.el
(use-package dash
  :straight t)

;; https://github.com/plexus/a.el/
;; alist functions
(use-package a
  :straight t)

;; https://github.com/rejeep/f.el/
;; file functions
(use-package f
  :straight t)

;; https://github.com/magnars/s.el/
;; string functions
(use-package s
  :straight t)

;; https://github.com/Wilfred/ht.el/
;; hash-table functions
(use-package ht
  :straight t)

;; https://github.com/alphapapa/ts.el
;; date and time functions
(use-package ts
  :straight t)

(provide 'config::helpers)

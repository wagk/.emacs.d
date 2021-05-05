;;; init.el --- Bootstrap further configurations -*- no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(setq package-enable-at-startup nil)

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (setq comp-deferred-compilation t))

;; (when (boundp 'auto-compile-on-load-mode))
  ;; auto-compile exists

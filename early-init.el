;;; init.el --- Bootstrap further configurations -*- no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(setq package-enable-at-startup nil)

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (setq comp-deferred-compilation t))

(defvaralias 'comp-deferred-compilation-deny-list 'native-comp-deferred-compilation-deny-list)

;; (when (boundp 'auto-compile-on-load-mode))
  ;; auto-compile exists

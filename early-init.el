;;; init.el --- Bootstrap further configurations -*- no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(setq package-enable-at-startup nil)

(add-to-list 'default-frame-alist '(undecorated . t))

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq comp-deferred-compilation t)
  (setq native-comp-async-report-warnings-errors 'silent))

(defvaralias
  'comp-deferred-compilation-deny-list
  'native-comp-deferred-compilation-deny-list)

(setq ;; gc-cons-threshold most-positive-fixnum
      garbage-collection-messages t)

;; (when (boundp 'auto-compile-on-load-mode))
  ;; auto-compile exists

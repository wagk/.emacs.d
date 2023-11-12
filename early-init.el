;;; init.el --- Bootstrap further configurations -*- no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(setq package-enable-at-startup nil)

(when (eq system-type 'windows-nt)
  (setq native-comp-speed -1)
  (setq native-comp-enable-subr-trampolines nil))

(unless (eq system-type 'windows-nt)
  (add-to-list 'default-frame-alist '(undecorated . t)))

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors 'silent))

(defvaralias
  'comp-deferred-compilation-deny-list
  'native-comp-deferred-compilation-deny-list)

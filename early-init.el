;;; init.el --- Bootstrap further configurations -*- no-byte-compile: t -*-
;;; Commentary:
;;; Code:
(setq package-enable-at-startup nil)

;; (when (bound-and-true-p 'package-install-upgrade-built-in)
;;   (setq package-install-upgrade-built-in t))

;; (when (eq system-type 'windows-nt)
;;   (setq native-comp-speed -1)
;;   (setq no-native-compile t)
;;   (setq native-comp-enable-subr-trampolines nil)
;;   (setq native-comp-jit-compilation nil))

(unless (eq system-type 'windows-nt)
  (add-to-list 'default-frame-alist '(undecorated . t)))

(setq native-comp-always-compile t)

;; (when (and (fboundp 'native-comp-available-p)
;;            (native-comp-available-p))
;;   (setq native-comp-async-report-warnings-errors 'silent)
;;   (when (boundp 'native-comp-always-compile)
;;     (setq native-comp-always-compile t)))

;; (defvaralias
;;   'comp-deferred-compilation-deny-list
;;   'native-comp-deferred-compilation-deny-list)

(tool-bar-mode -1)
(menu-bar-mode -1)
(when (boundp 'scroll-bar-mode) (scroll-bar-mode -1))
(window-divider-mode -1)

(defcustom --default-font-size 20
  "Default frame font size."
  :group 'personal
  :type 'natnum
  :set
  #'(lambda (obj size)
      (set-default-toplevel-value obj size)
      (with-eval-after-load 'config-theme
        (--find-and-set-fonts))))

;; cl-defun is not loaded here
(defun --find-and-set-fonts ()
  (interactive)
  (let ((sarasa-mono (font-spec :family "Sarasa Mono J" :size --default-font-size))
        (iosevka (font-spec :family "Iosevka" :size --default-font-size))
        (iosevka-etoile (font-spec :family "Iosevka Etoile" :size --default-font-size))
        (iosevka-aile (font-spec :family "Iosevka Aile" :size --default-font-size))
        (courier (font-spec :family "Courier" :size --default-font-size)))
    (cond
     ((find-font sarasa-mono)
      (set-frame-font sarasa-mono nil t)
      (custom-set-faces `(fixed-pitch ((default . (:font ,sarasa-mono))))
                        `(variable-pitch ((default . (:font ,sarasa-mono))))
                        `(fixed-pitch-serif ((default . (:font ,sarasa-mono))))))
     ((find-font iosevka)
      (set-frame-font iosevka nil t)
      (custom-set-faces `(fixed-pitch ((default . (:font ,iosevka))))
                        `(variable-pitch ((default . (:font ,iosevka))))
                        `(fixed-pitch-serif ((default . (:font ,iosevka)))))))
    ;; If iosevka proportional fonts are also found, use that.
    (when (find-font iosevka-etoile)
      (custom-set-faces `(variable-pitch ((default . (:font ,iosevka-etoile))))))
    (when (find-font iosevka-aile)
      (custom-set-faces `(fixed-pitch-serif ((default . (:font ,iosevka-aile))))))))

(--find-and-set-fonts)

;; (add-hook 'after-init-hook #'--find-and-set-fonts)
(add-hook 'server-after-make-frame-hook #'--find-and-set-fonts)

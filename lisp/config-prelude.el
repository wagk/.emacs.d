;; Load core configuration that I can't work without.

;; Package stuff begin

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f seconds." (float-time (time-since time)))))

(defun bootstrap-package ()
  "Add package repositories and call `package-initialize'."
  (when package-enable-at-startup
    (require 'package)
    (dolist (x '(("melpa"        . "https://melpa.org/packages/")
                 ("melpa-2"      . "https://melpa.milkbox.net/packages/")
                 ("melpa-stable" . "https://stable.melpa.org/packages/")
                 ("elpy"         . "https://jorgenschaefer.github.io/packages/")
                 ("gnu"          . "https://elpa.gnu.org/packages/")
                 ("gnu-devel"    . "https://elpa.gnu.org/devel/")
                 ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                 ("marmalade"    . "https://marmalade-repo.org/packages/")))
      (add-to-list 'package-archives x)))
  (when (< emacs-major-version 27)
    ;; package-initialize doesn't have to be called here in emacs 27
    (package-initialize)))

(defun bootstrap-straight ()
  "Load straight.el, downloading it if necessary.
 `package-initialize' must be called prior to this."
  ;; Requires (package-initialize) to be called
  ;; https://github.com/raxod502/straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (customize-set-variable 'straight-use-package-by-default t))

;;; Use package
(defun bootstrap-use-package ()
  "Check if use-package is installed and install it if it isn't.
 Then performs configuration of `use-package' variables."
  ;; (unless (featurep 'quelpa)
  ;;   (bootstrap-quelpa))
  ;; (quelpa
  ;;   '(quelpa-use-package
  ;;     :fetcher git
  ;;     :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))
  ;; (require 'quelpa-use-package)
  (require 'straight)
  (straight-use-package '(use-package
                           :host github
                           :repo "jwiegley/use-package")))

(require 'config-elpaca)

;; (bootstrap-use-package)

(elpaca elpaca-use-package
    (elpaca-use-package-mode))

(elpaca-wait)

(customize-set-variable 'load-prefer-newer t)
(require 'use-package)
;; download packages if needed
(customize-set-variable 'use-package-always-defer nil
                        "we don't always lazy load because of explicitness")
(customize-set-variable 'use-package-always-ensure t)
(customize-set-variable 'use-package-verbose t)
(customize-set-variable 'use-package-compute-statistics t)
(customize-set-variable 'use-package-hook-name-suffix nil)
(use-package use-package-ensure-system-package)
;; Package stuff end

;; https://github.com/magnars/dash.el
(use-package dash)

;; https://github.com/plexus/a.el/
;; alist functions
(use-package a)

;; https://github.com/rejeep/f.el/
;; file functions
(use-package f)

;; https://github.com/magnars/s.el/
;; string functions
(use-package s)

;; https://github.com/Wilfred/ht.el/
;; hash-table functions
(use-package ht)

;; https://github.com/alphapapa/ts.el
;; date and time functions
(use-package ts)

(use-package async)

(use-package general
  :ensure (:host github :repo "noctuid/general.el")
  :init
  (defconst my-default-evil-leader-key "SPC"))

(use-package no-littering
  :ensure (:host github :repo "emacscollective/no-littering"))

(use-package restart-emacs
  :if (not (eq system-type 'darwin))
  :ensure (:host github :repo "iqbalansari/restart-emacs")
  :commands (restart-emacs restart-emacs-start-new-emacs)
  :init
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "restart" 'restart-emacs)
    (evil-ex-define-cmd "restarttest"
                        'restart-emacs-start-new-emacs)))

(use-package blackout)

(elpaca-wait)

(let ((custom (locate-user-emacs-file "custom.el")))
  (unless (f-exists-p custom)
    (f-touch custom))
  (setq custom-file custom))

(provide 'config-prelude)

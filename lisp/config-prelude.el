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
    (elpaca-use-package-mode)
    (customize-set-variable 'load-prefer-newer t)
    ;; must be set before loading use-package
    (customize-set-variable 'use-package-enable-imenu-support t)
    ;; download packages if needed
    (customize-set-variable 'use-package-always-defer nil
                            "we don't always lazy load because of explicitness")
    (customize-set-variable 'use-package-always-ensure t)
    (customize-set-variable 'use-package-verbose t)
    (customize-set-variable 'use-package-compute-statistics t)
    (customize-set-variable 'use-package-hook-name-suffix nil)
    (require 'use-package))

;; Package stuff end

;; Garbage collection
(use-package gcmh
  :ensure (:host github :repo "emacsmirror/gcmh" :branch "master")
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 50)
  :config
  (gcmh-mode)
  (with-eval-after-load 'blackout
    (blackout 'gcmh-mode)))

(use-package compat
  :ensure (:host github :repo "emacs-straight/compat" :branch "master"))

(use-package blackout
  :ensure (:host github :repo "emacsmirror/blackout" :branch "master"))

(use-package general
  :ensure (:host github :repo "noctuid/general.el")
  :init
  (defconst my-default-evil-leader-key "SPC"))

;; More examples about transient can be found at:
;; https://github.com/positron-solutions/transient-showcase
(use-package transient
  :ensure (:host github :repo "magit/transient")
  ;; https://github.com/progfolio/elpaca/issues/236#issuecomment-1879838229
  ;; :preface
  ;; (unload-feature 'transient t)
  ;; (setq custom-delayed-init-variables '())
  :after config-theme
  :custom-face
  (transient-key-exit
   ((default . (:foreground ,sol-red))))
  (transient-key-return
   ((default . (:foreground ,sol-yellow))))
  (transient-key-stay
   ((default . (:foreground ,sol-blue)))))

(elpaca-wait)

;; https://github.com/magnars/dash.el
(use-package dash
  :ensure (:host github :repo "emacsmirror/dash" :branch "master"))

;; https://github.com/plexus/a.el/
;; alist functions
(use-package a
  :ensure (:host github :repo "emacsmirror/a" :branch "master"))

;; https://github.com/rejeep/f.el/
;; file functions
(use-package f
  :ensure (:host github :repo "emacsmirror/f" :branch "master"))

;; https://github.com/magnars/s.el/
;; string functions
(use-package s
  :ensure (:host github :repo "emacsmirror/s" :branch "master"))

;; https://github.com/Wilfred/ht.el/
;; hash-table functions
(use-package ht
  :ensure (:host github :repo "emacsmirror/ht" :branch "master"))

;; https://github.com/alphapapa/ts.el
;; date and time functions
(use-package ts
  :ensure (:host github :repo "emacsmirror/ts" :branch "master"))

(use-package async
  :ensure (:host github :repo "emacsmirror/async" :branch "master"))

(use-package no-littering
  :ensure (:host github :repo "emacscollective/no-littering")
  :after f
  :config
  (setq backup-directory-alist `(("." .  ,(file-name-concat
                                           no-littering-etc-directory
                                           "backups"))))
  (let ((dir (f-join no-littering-etc-directory "auto-save/")))
    (unless (f-exists-p dir)
      (f-mkdir dir))
    (add-to-list 'auto-save-file-name-transforms
                 `(".*" ,dir t))))

;; (let ((custom (locate-user-emacs-file "custom.el")))
;;   (unless (f-exists-p custom)
;;     (f-touch custom))
;;   (setq custom-file custom))

(provide 'config-prelude)

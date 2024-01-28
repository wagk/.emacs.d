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
  (customize-set-variable 'load-prefer-newer t)
  (straight-use-package '(use-package
                           :host github
                           :repo "jwiegley/use-package"))
  (require 'use-package)
  ;; download packages if needed
  (customize-set-variable 'use-package-always-defer nil
                          "we don't always lazy load because of explicitness")
  (customize-set-variable 'use-package-always-ensure nil
                          "always make sure it never skips if not
                          found. Disabled because we want straight to
                          do the heavy lifting")
  (customize-set-variable 'use-package-verbose t)
  (customize-set-variable 'use-package-compute-statistics t)
  (customize-set-variable 'use-package-hook-name-suffix nil)
  (use-package use-package-ensure-system-package))

(cl-defun bootstrap-elpaca ()
  (defvar elpaca-installer-version 0.6)
  (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
  (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
  (defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
  (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                                :ref nil
                                :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                                :build (:not elpaca--activate-package)))
  (let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
         (build (expand-file-name "elpaca/" elpaca-builds-directory))
         (order (cdr elpaca-order))
         (default-directory repo))
    (add-to-list 'load-path (if (file-exists-p build) build repo))
    (unless (file-exists-p repo)
      (make-directory repo t)
      (when (< emacs-major-version 28) (require 'subr-x))
      (condition-case-unless-debug err
          (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                   ((zerop (call-process "git" nil buffer t "clone"
                                         (plist-get order :repo) repo)))
                   ((zerop (call-process "git" nil buffer t "checkout"
                                         (or (plist-get order :ref) "--"))))
                   (emacs (concat invocation-directory invocation-name))
                   ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                         "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                   ((require 'elpaca))
                   ((elpaca-generate-autoloads "elpaca" repo)))
              (progn (message "%s" (buffer-string)) (kill-buffer buffer))
            (error "%s" (with-current-buffer buffer (buffer-string))))
        ((error) (warn "%s" err) (delete-directory repo 'recursive))))
    (unless (require 'elpaca-autoloads nil t)
      (require 'elpaca)
      (elpaca-generate-autoloads "elpaca" repo)
      (load "./elpaca-autoloads")))
  (add-hook 'after-init-hook #'elpaca-process-queues))
  ;; (elpaca `(,@elpaca-order)))
  ;; (elpaca-wait)

(elpaca-no-symlink-mode)
(bootstrap-elpaca)
(bootstrap-use-package)

(elpaca elpaca-use-package
    (elpaca-use-package-mode)
    (setq elpaca-use-package-by-default t))
(elpaca-wait)
;; Package stuff end

;; https://github.com/magnars/dash.el
(use-package dash :elpaca t)

;; https://github.com/plexus/a.el/
;; alist functions
(use-package a :elpaca t)

;; https://github.com/rejeep/f.el/
;; file functions
(use-package f :elpaca t)

;; https://github.com/magnars/s.el/
;; string functions
(use-package s :elpaca t)

;; https://github.com/Wilfred/ht.el/
;; hash-table functions
(use-package ht :elpaca t)

;; https://github.com/alphapapa/ts.el
;; date and time functions
(use-package ts :elpaca t)

(use-package async :elpaca t)

(use-package general
  :elpaca (:host github :repo "noctuid/general.el")
  :init
  (defconst my-default-evil-leader-key "SPC"))

(use-package no-littering
  :elpaca (:host github :repo "emacscollective/no-littering"))

(use-package restart-emacs
  :if (not (eq system-type 'darwin))
  :elpaca (:host github :repo "iqbalansari/restart-emacs")
  :commands (restart-emacs restart-emacs-start-new-emacs)
  :init
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "restart" 'restart-emacs)
    (evil-ex-define-cmd "restarttest"
                        'restart-emacs-start-new-emacs)))

(use-package blackout :elpaca t)

(let ((custom (locate-user-emacs-file "custom.el")))
  (unless (f-exists-p custom)
    (f-touch custom))
  (setq custom-file custom))

(provide 'config-prelude)

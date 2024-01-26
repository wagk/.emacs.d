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
  (customize-set-variable 'straight-repository-branch "develop")
  (customize-set-variable 'straight-use-package-by-default t)
  (customize-set-variable 'straight-check-for-modifications
                          '(check-on-save find-when-checking))
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el"
                           user-emacs-directory))
        (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (message "Bootstrapping Straight.el...")
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

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

(defun load-config-org-files (files)
  "Given a list of org FILES, load them sequentially in the order.
 specified The list of files is assumed to be relative to
 `user-init-dir' TODO: Error checking; relative pathing, error
 recovery. Maybe eventually load dependencies and all that."
  (dolist (file files)
    (message "Loading %s" file)
    (condition-case nil
        (org-babel-load-file (locate-user-emacs-file file))
      (error (message "There was an error when loading %s" file)))))

(cl-defun --completing-read (prompt collection
                                    &key predicate require-match
                                    initial-input history default-value
                                    inherit-input-method)
  "Wrapper around `completing-read' that allow the use of keywords."
  (completing-read prompt collection
                   predicate
                   require-match
                   initial-input
                   history
                   default-value
                   inherit-input-method))

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
(customize-set-variable 'straight-use-package-by-default t)

(bootstrap-package)
(bootstrap-use-package)

;; Package stuff end

;; https://github.com/magnars/dash.el
(use-package dash :straight t)

;; https://github.com/plexus/a.el/
;; alist functions
(use-package a :straight t)

;; https://github.com/rejeep/f.el/
;; file functions
(use-package f :straight t)

;; https://github.com/magnars/s.el/
;; string functions
(use-package s :straight t)

;; https://github.com/Wilfred/ht.el/
;; hash-table functions
(use-package ht :straight t)

;; https://github.com/alphapapa/ts.el
;; date and time functions
(use-package ts :straight t)

(use-package async :straight t)

(use-package general
  :straight (:host github :repo "noctuid/general.el")
  :init
  (defconst my-default-evil-leader-key "SPC"))

(use-package no-littering
  :straight (:host github :repo "emacscollective/no-littering"))

(use-package restart-emacs
  :if (not (eq system-type 'darwin))
  :straight (:host github :repo "iqbalansari/restart-emacs")
  :commands (restart-emacs restart-emacs-start-new-emacs)
  :init
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "restart" 'restart-emacs)
    (evil-ex-define-cmd "restarttest"
                        'restart-emacs-start-new-emacs)))

(use-package blackout :straight t)

(let ((custom (locate-user-emacs-file "custom.el")))
  (unless (f-exists-p custom)
    (f-touch custom))
  (setq custom-file custom))

(provide 'config-prelude)

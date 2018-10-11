;;; init.el --- Bootstrap further configurations

;;; Commentary:
;; Useful resources:
;; https://bling.github.io/blog/2013/10/27/emacs-as-my-leader-vim-survival-guide/
;; https://github.com/bbatsov/emacs-lisp-style-guide
;; https://github.com/noctuid/evil-guide
;; TODO: Figure out why there is a eager-macro expansion failure

;; Latest builds can be found at:: alpha.gnu.org/gnu/emacs/pretest/windows/
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/

;;; Code:

;; Note that docstrings for variables come *after* the value

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(setq user-full-name    "Pang Tun Jiang"
      user-mail-address "pang.tun.jiang@gmail.com")

;; buffer encoding systems
;; We do this here because the package system might need to know our preferences
(prefer-coding-system 'utf-8)

(defconst user-init-dir
  (file-name-as-directory
   (cond ((boundp 'user-emacs-directory)
	  user-emacs-directory)
	 ((boundp 'user-init-directory)
	  user-init-directory)
	 (t "~/.emacs.d/")))
  "Sets up the startup directory.")

(defun at-user-init-dir (filename)
  "Convenience function for files that path relative to `user-init-dir'"
  (expand-file-name (concat user-init-dir filename)))

(defconst user-init-file
  (at-user-init-dir "init.el")
  "Points to init.el")

(defconst user-config-file
  (at-user-init-dir "config.org")
  "Points to config.org")

(defconst user-local-file
  (at-user-init-dir "local.el")
  "Points to local.el")

(defconst user-config-file-list
  '("core.org"
    "lang.org")
  "List of config files that are to be loaded. Load order is the
  sequence defined within the list")

(defun find-user-init-file ()
  "Edit `user-init-file' without opening a new window."
  (interactive)
  (find-file user-init-file))

(defun find-user-config-file ()
  "Edit `user-config-file' without opening a new window."
  (interactive)
  (find-file user-config-file))

(defun find-user-local-file ()
  "Edit `local.el' without opening a new window."
  (interactive)
  (find-file user-local-file))

(defun find-message-buffer ()
  "Go to the *Messages* buffer"
  (interactive)
  (switch-to-buffer "*Messages*"))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f seconds." (float-time (time-since time)))))

(defun my-bootstrap-package ()
  "Adds package repositories and calls `package-initialize'"
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa-2" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
  (add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")) ; https://marmalade-repo.org/packages/#windowsinstructions
  (package-initialize))

(defun my-bootstrap-straight ()
  ;; https://github.com/raxod502/straight.el
  (let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
        (bootstrap-version 3))
    (unless (file-exists-p bootstrap-file)
      (message "Bootstrapping Straight.el...")
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (setq straight-cache-autoloads t))

(defun my-bootstrap-use-package ()
  "Checks if use-package is installed and installs it if it isn't.
  Then performs configuration of use-package variables"
  (unless (featurep 'straight)
    (my-bootstrap-straight))
  (require 'straight)
  (customize-set-variable 'load-prefer-newer t)
  (straight-use-package '(use-package
                             :type git
                             :host github
                             :repo "jwiegley/use-package"
                             :branch "master"))
  ;; TODO: eval-and-compile-ing this sexp causes some eager macro expansion warning
  (require 'use-package)
  ;; download packages if needed
  ;; this is disabled because I feel that verbose is better
  ;; (setq use-package-always-ensure t)
  (setq use-package-always-defer t ;; always lazy load
        use-package-always-ensure t ;; always make sure it never skips if not found
        use-package-verbose t
        use-package-compute-statistics nil))

(defun my-ensure-local-el-file-exists ()
  "Checks if there exists a local.el file. Creates one if it doesn't
exist, using the template specified in
'auto-insert/elisp-local-template'"
  (let ((local-file (at-user-init-dir "local.el")))
    (unless (file-exists-p local-file)
      ;; output a templated local.el file into local.el
      (write-region (with-temp-buffer
                      (insert-file-contents (at-user-init-dir)))
               "auto-insert/elisp-local-template"
                      (buffer-string)
                    nil local-file))))

(defun my-load-config-org-files (files)
  "Given a list of org files, loads them sequentially in the order specified
The list of files is assumed to be relative to `user-init-dir'
TODO: Error checking; relative pathing, error recovery. Maybe
eventually load dependencies and all that."
  (dolist (file files)
    (message "Loading %s" file)
    (condition-case nil
        (org-babel-load-file (at-user-init-dir file))
      (error (message "There was an error when loading %s" file)))))

(let ((gc-cons-threshold most-positive-fixnum))
  (my-bootstrap-package)
  (my-bootstrap-straight)
  (my-bootstrap-use-package)

  ;;NOTE: Do *NOT* compile this, certain macro definitions won't get compiled
  ;;and the init load will fail
  (measure-time
   (my-load-config-org-files user-config-file-list))

  ;; (straight-freeze-versions)

  ;; Disable ANNOYING customize options
  (setq custom-file (at-user-init-dir "custom.el"))
  (load custom-file 'noerror))

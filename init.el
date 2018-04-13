;;; init.el --- Bootstrap further configurations

;;; Commentary:
;; Useful resources:
;; https://bling.github.io/blog/2013/10/27/emacs-as-my-leader-vim-survival-guide/
;; https://github.com/bbatsov/emacs-lisp-style-guide
;; https://github.com/noctuid/evil-guide

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
   (cond ((boundp 'user-emacs-directory) user-emacs-directory)
         ((boundp 'user-init-directory) user-init-directory)
         (t "~/.emacs.d/"))
   )
  "Sets up the startup directory.")

(defconst user-init-file
  (concat user-init-dir "init.el")
  "Points to init.el")

(defconst user-config-file
  (concat user-init-dir "config.org")
  "Points to config.org")

(defconst user-local-file
  (concat user-init-dir "local.el")
  "Points to local.el")

;; (defconst user-config-dir
;;   (file-name-as-directory
;;    (concat user-init-dir "config"))
;;   "Directory where all the user configuration files are stored")

;;;###autoload
(defun find-user-init-file ()
  "Edit `user-init-file' without opening a new window."
  (interactive)
  (find-file user-init-file)
  )

;;;###autoload
(defun find-user-config-file ()
  "Edit `user-config-file' without opening a new window."
  (interactive)
  (find-file user-config-file)
  )

;;;###autoload
(defun find-user-local-file ()
  "Edit `local.el' without opening a new window."
  (interactive)
  (find-file user-local-file)
  )


(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f seconds." (float-time (time-since time)))))

;; TODO(pangt): make this take in relative paths
(defun load-user-config-file (file &rest files)
  "Load FILE (and FILES) as configuration.
Assumes that it:
- Is a string path to one or more configuration fila (i.e. elisp)
- Is relative to user-config-dir"
  (interactive "fConfig file: ")
  (measure-time
   (dolist (elem (cons file files))
     (let ((path (expand-file-name (concat user-config-dir elem))))
       (if (file-exists-p path)
           (progn (condition-case nil
                      (measure-time (load-file path))
                    (error
                     (message "There was an error while loading %s" elem)))
                  (message "Loaded %s" path))
         (message "Failed to load %s" path))))))

;; Add to load path our configuration folder. Deprecated since we don't use
;; a config directory anymore
;; (add-to-list 'load-path user-config-dir)

(let ((gc-cons-threshold most-positive-fixnum))

  (require 'package)

  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa-2" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
  (add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")) ; https://marmalade-repo.org/packages/#windowsinstructions

  ;; el-get stuff
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")

  (unless (require 'el-get nil 'noerror)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp)))

  (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
  (el-get 'sync)

                                        ; Got a warning regarding golden-ratio when I loaded this before el-get, for
  ;; some reason
  ;; TODO; figure out what this does
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (progn (eval-when-compile (require 'use-package)))

  ;; download packages if needed
  ;; this is disabled because I feel that verbose is better
  ;; (setq use-package-always-ensure t)
  (setq use-package-always-defer t ;; always lazy load
        use-package-always-ensure t ;; always make sure it never skips if not found
        use-package-verbose t
        use-package-compute-statistics t)

  (use-package diminish)
  (use-package bind-key)

  ;; https://github.com/noctuid/general.el
  (use-package general
    :demand t
    :commands (general-define-key)
    :init
    (defconst my-default-evil-leader-key "SPC")
    )

  (use-package use-package-el-get
    :demand t
    :config
    (use-package-el-get-setup))

  (use-package use-package-ensure-system-package
    :demand t)

  ;; ;; be aware that updates might adjust the load path to the .el files and
  ;; ;; cause loading problems. Helm seems to be a victim of this a lot
  ;; (use-package spu
  ;;   :disabled t
  ;;   :defer 5 ;; defer package loading for 5 second
  ;;   :config
  ;;   ;; attempt to upgrade packages only when we're leaving
  ;;   (add-hook 'kill-emacs-hook 'spu-package-upgrade))

  ;; https://github.com/emacscollective/auto-compile
  (use-package auto-compile
    :demand t
    :init
    (setq load-prefer-newer t)
    :config
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)
    )

  (use-package auto-package-update
    :demand t
    :commands (auto-package-update-now
               auto-package-update-at-time
               auto-package-update-maybe)
    :custom
    (auto-package-update-delete-old-versions t "We already version them on git")
    (auto-package-update-prompt-before-update t "NO SURPRISES")
    (auto-package-update-interval 14 "update once every 2 weeks (the count is in days)"))

  (let ((local-file (concat user-init-dir "local.el")))
    (unless (file-exists-p local-file)
      ;; output a templated local.el file into local.el
      (write-region (with-temp-buffer
                      (insert-file-contents (concat user-init-dir
                                                    "auto-insert/elisp-local-template"))
                      (buffer-string))
                    nil local-file)))

  ;; local configuration variables
  (load (concat user-init-dir "local.el") 'noerror)

  ;; We assume we can call use-package multiple times
  ;; TODO: configure these meaningfully
  (use-package org)
  ;; (use-package evil)
  ;; (use-package helm)

  ;;NOTE: Do *NOT* compile this, certain macro definitions won't get compiled
  ;;and the init load will fail
  (measure-time
   (org-babel-load-file
    (expand-file-name (concat user-init-dir "config.org"))))

  ;; Disable ANNOYING customize options
  (setq custom-file (concat user-init-dir "custom.el"))
  (load custom-file 'noerror)
  )

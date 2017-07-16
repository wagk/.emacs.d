;;; init.el --- Bootstrap further configurations

;;; Commentary:
;; Useful resources:
;; https://bling.github.io/blog/2013/10/27/emacs-as-my-leader-vim-survival-guide/
;; https://github.com/bbatsov/emacs-lisp-style-guide

;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/

;;; Code:

;; Note that docstrings for variables come *after* the value

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory) user-emacs-directory)
        ((boundp 'user-init-directory) user-init-directory)
        (t "~/.emacs.d/"))
  "Sets up the startup directory.")

(defconst user-config-dir
  (concat user-init-dir "config/"))

(defun load-user-config-file (file &rest files)
  "Load FILE (and FILES) as configuration.
Assumes that it:
- Is a string path to one or more configuration fila (i.e. elisp)
- Is relative to user-init-dir"
  (interactive "f")
  (dolist (elem (cons file files))
    (load-file (expand-file-name (concat user-init-dir elem)))))

;; Add to load path our configuration folder
(add-to-list 'load-path user-config-dir)

;; tweak garbage collector before
(defvar default-gc-cons-threshold 20000000)
(setq gc-cons-threshold most-positive-fixnum)

;; Disable ANNOYING customize options
(setq custom-file (concat user-init-dir "custom.el"))
(load custom-file 'noerror)

;; load each config file in order
(load-user-config-file "config/config-utility.el"
                       "config/config-package.el"
                       "config/config-common.el"
                       "config/config-evil.el"
                       "config/config-helm.el"
                       "config/config-buffer.el"
                       "config/config-git.el"
                       "config/config-org.el"
                       "config/config-project.el"
                       "config/config-completion.el"
                       "config/config-webdev.el"
                       "config/config-help.el"
                       "config/config-lisp.el"
                       )

(setq gc-cons-threshold default-gc-cons-threshold)

;; -----------------------------------------------------------------------------
;; UNCONFIGURED CONFIGURATION CODE BELOW
;; -----------------------------------------------------------------------------
(setq use-package-always-ensure t)

;; crypto
(require 'epa-file)
(epa-file-enable)

(use-package powershell)

;; be aware that updates might adjust the load path to the .el files and
;; cause loading problems. Helm seems to be a victim of this a lot
(use-package spu
  :defer 5 ;; defer package loading for 5 second
  :config
  ;; attempt to upgrade packages only when we're leaving
  (add-hook 'kill-emacs-hook 'spu-package-upgrade))

;; Japanese mode
(require 'kkc)
(eval-after-load "kkc"
  (progn
    (setq default-input-method "japanese"
          kkc-show-conversion-list-count 1)
    ;; (define-key kkc-keymap (kbd "SPC")       'kkc-terminate)
    ;; (define-key kkc-keymap (kbd "<tab>")     'kkc-next)
    ;; (define-key kkc-keymap (kbd "<backtab>") 'kkc-prev)
    )
  )

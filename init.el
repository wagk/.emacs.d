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
                       "config/config-help.el")

(setq gc-cons-threshold default-gc-cons-threshold)











































;;; package --- Summary
;;; Commentary:
;;; Code:
;; TODO(pangt): chunk the config file into separate packages


;; (defvar default-gc-cons-threshold gc-cons-threshold)

(defvar default-gc-cons-threshold 20000000)

(defun my-minibuffer-setup-hook ()
  "Set the garbage collection threshold to the maximum."
  (setq gc-cons-threshold most-positive-fixnum)
  )

(defun my-minibuffer-exit-hook ()
  "Set the garbage collection threshold to the default."
  (setq gc-cons-threshold default-gc-cons-threshold)
  )

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; While initing, max out gc threshold
(setq gc-cons-threshold most-positive-fixnum)

(defun user-emacs-subdirectory (subdir)
  "Not sure if needed, but I don't like how arbitrary `~/.emacs.d/` is.
SUBDIR should not have a `/` in front."
  (concat user-emacs-directory subdir)
  )

(setq use-package-always-ensure t)

;; TODO(pangt): figure out how this works
(defun text-file-p (filename)
  "Check if the FILENAME is a binary file."
  (with-current-buffer (find-file-noselect filename :no-warn)
    (prog1 (not (eq buffer-file-coding-system 'no-conversion))
      (kill-buffer))))

(use-package rainbow-delimiters
  :config
  ;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

(use-package undo-tree)
(use-package goto-chg)

;; crypto
(require 'epa-file)
(epa-file-enable)

(use-package helm-emmet)

(use-package powershell)

(if (bound-and-true-p highlight-indentation-mode)
    (highlight-indentation-mode))

;; be aware that updates might adjust the load path to the .el files and
;; cause loading problems. Helm seems to be a victim of this a lot
(use-package spu
  :defer 5 ;; defer package loading for 5 second
  :config
  ;; attempt to upgrade packages only when we're leaving
  (add-hook 'kill-emacs-hook 'spu-package-upgrade))

(require 'tramp)
(cond ((eq system-type "windows-nt")
       (progn (setq tramp-default-method "plink"))))
(use-package tramp-term)
(use-package docker-tramp)

(use-package which-key
  :config
  ;; (setq which-key-popup-type 'minibuffer)
  (which-key-mode 0)
  )

(use-package elisp-slime-nav
  :config
  (defun my-elisp-mode()
    (elisp-slime-nav-mode)
    (turn-on-eldoc-mode))
  (add-hook 'emacs-lisp-mode-hook 'my-elisp-mode)
  )

(use-package transpose-frame)
(use-package buffer-move)
(use-package unicode-troll-stopper)

(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\Jenkinsfile\\'" . groovy-mode))

;; startup maximised
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

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

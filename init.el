(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; turn on line numbers
(global-linum-mode 1)

;; startup maximised
(add-to-list 'default-frame-alist '(fullscreen . maximised))

;; autopairing
(electric-pair-mode 1)

;; autoindentation
(electric-indent-mode 1)

					; Remove toolbar
(tool-bar-mode -1)

;; set default font
(add-to-list 'default-frame-alist '(font . "Consolas-11"))

;;datetime things
(defvar current-date-time-format "%Y-%m-%dT%H:%M:%S"
  "Format of date to insert with `insert-current-date-time' func
  See help of `format-time-string' for possible replacements")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
  Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-time-format (current-time))))

(defun find-user-init-file-here ()
  "Edit `user-init-file` without opening a new window"
  (interactive)
  (find-file user-init-file)
  )

(defun find-user-init-file ()
  "Edit `user-init-file` in another window"
  (interactive)
  (find-file-other-window user-init-file)
  )





(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-2" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/")) ;
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/")) ; https://marmalade-repo.org/packages/#windowsinstructions
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  )
;; (package-refresh-contents)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  )

(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t) ;; make sure we download when necessary


(use-package el-get
  :init
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
  :config
  (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
  (el-get 'sync)
  )

(use-package elpy
  :config
  (elpy-enable); TODO: configure elpy
  )

;; ;; Example el-get-sources
;; (setq el-get-sources
;;       '(:name yasnippet
;; 	      :type github
;; 	      :pkgname "joaotavora/yasnippet"
;; 	      :after (yas-global-mode 1)
;; 	      )
;;       )


(use-package yasnippet
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets") ;; personal snippets
	)
  (yas-global-mode 1)
  )

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)

  (use-package evil-leader
    :init
    (add-to-list 'load-path "~/.emacs.d/packages/evil-leader")
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "<SPC>"	'helm-M-x
      "\\"	'magit-status
      "f"	'find-file
      "t"	'insert-current-date-time
      "cc"	'comment-or-uncomment-region
      "a"	'align-regexp
      )
    )

  :config
  (evil-mode 1)

  (use-package evil-surround
    :config
    (global-evil-surround-mode 1)
    (setq-default evil-surround-pairs-alist (cons '(?~ . ("~" . "~"))
						  evil-surround-pairs-alist)))

  (use-package evil-args
    :config
    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "i" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
    ;; bind evil-forward/backward-args
    (define-key evil-normal-state-map "L" 'evil-forward-arg)
    (define-key evil-normal-state-map "H" 'evil-backward-arg)
    (define-key evil-motion-state-map "L" 'evil-forward-arg)
    (define-key evil-motion-state-map "H" 'evil-backward-arg)
    ;; bind evil-jump-out-args
    (define-key evil-normal-state-map "K" 'evil-jump-out-args)
    )

  (use-package evil-numbers
    :config
    (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
    )
  
  (use-package evil-vimish-fold
    :config
    (evil-vimish-fold-mode 1)
    )
  
  (use-package evil-commentary)
  (use-package evil-replace-with-register)
  (use-package evil-text-object-python)
  (use-package evil-magit)
  (use-package evil-tabs
    :init
    (use-package elscreen)
    :config
    (global-evil-tabs-mode t)
    )

  (evil-set-initial-state 'info-mode 'normal)
  (setq evil-normal-state-modes (append evil-motion-state-modes evil-normal-state-modes))
  (setq evil-motion-state-modes nil)
  (define-key global-map (kbd "C-f") 'universal-argument)
  (define-key universal-argument-map (kbd "C-u") nil)
  (define-key universal-argument-map (kbd "C-f") 'universal-argument-more)
  (define-key global-map (kbd "C-u") 'kill-whole-line)
  (eval-after-load 'evil-maps
    '(progn
       (define-key evil-motion-state-map (kbd "C-f") nil)
       (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)
       )
    )
  ;; change mode-line color by evil state
  (lexical-let ((default-color (cons (face-background 'mode-line)
				     (face-foreground 'mode-line)))
		)
    (add-hook 'post-command-hook
	      (lambda ()
		(let ((color (cond ((minibufferp) default-color)
				   ((evil-insert-state-p) '("#e80000" . "#ffffff"))
				   ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
				   ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
				   (t default-color))))
		  (set-face-background 'mode-line (car color))
		  (set-face-foreground 'mode-line (cdr color)))))
    )
  )

;; rebind <C-u> to intended behavior, otherwise defaults to universal-argument

;; (use-package evil-surround
;;     :config
;;     (global-evil-surround-mode 1)
;;     (setq-default evil-surround-pairs-alist (cons '(?~ . ("~" . "~"))
;; 						  evil-surround-pairs-alist)))

;; (use-package evil-args
;;     :config
;;     ;; bind evil-args text objects
;;     (define-key evil-inner-text-objects-map "i" 'evil-inner-arg)
;;     (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
;;     ;; bind evil-forward/backward-args
;;     (define-key evil-normal-state-map "L" 'evil-forward-arg)
;;     (define-key evil-normal-state-map "H" 'evil-backward-arg)
;;     (define-key evil-motion-state-map "L" 'evil-forward-arg)
;;     (define-key evil-motion-state-map "H" 'evil-backward-arg)
;;     ;; bind evil-jump-out-args
;;     (define-key evil-normal-state-map "K" 'evil-jump-out-args)
;;     )

;; (use-package evil-numbers
;;     :config
;;     (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
;;     (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
;;     )

;; evil number support
;;(require 'evil-numbers)
;;(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
;;(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

;; ;; evil leader support
;; (use-package evil-commentary)
;; (use-package evil-replace-with-register)
;; (use-package evil-text-object-python)
;; (use-package evil-magit)
;; (use-package evil-tabs
;;   :init
;;   (use-package elscreen)
;;   :config
;;   (global-evil-tabs-mode t)
;;   )

;; orgmode bindings
(use-package org-evil
  :config
  (setq org-M-RET-may-split-line nil) ;; so we can press 'o' in evil and generate the next item
  )

;; activate helm mode
(use-package helm
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)
  )

;;solarized dark theme
(use-package solarized-theme
  :config
  (setq solarized-use-variable-pitch nil
	solarized-scale-org-headlines nil) ;;unscrew org layout
  (load-theme 'solarized-dark t)
  )

(use-package powershell)

(use-package company
  :config

  (use-package company-quickhelp
    :config
    (setq company-quickhelp-idle-delay 0)
    )

  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
	      '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  (use-package helm-company)
  (setq company-idle-delay 0)
  (setq company-require-match nil)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  ;(define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (add-hook 'after-init-hook 'global-company-mode)
  )

(use-package tramp-term)

(use-package docker-tramp)

(use-package discover)

(use-package groovy-mode)

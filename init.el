;;; package --- Summary
;;; Commentary:
;;; Code:

;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/

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

;;datetime things
(defvar current-date-time-format "%Y-%m-%dT%H:%M:%S"
  "Format of date to insert with `insert-current-date-time' func.
See help of `format-time-string' for possible replacements")

(defun insert-current-date-time ()
  "Insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-time-format (current-time))))

(defun find-user-init-file ()
  "Edit `user-init-file` without opening a new window."
  (interactive)
  (find-file user-init-file)
  )

(defun user-emacs-subdirectory (subdir)
  "Not sure if needed, but I don't like how arbitrary `~/.emacs.d/` is.
SUBDIR should not have a `/` in front."
  (concat user-emacs-directory subdir)
  )

;; (defun my-japanese-input-toggle ()
;;   "Rotates the input method between none -> hiragana -> katakana."
;;   (when (or (equal current-input-method "japanese-hiragana")
;;             (equal default-input-method "japanese-hiragana"))
;;     (set-input-method "japanese-katakana"))
;;   (when (or (equal current-input-method "japanese-katakana")
;;             (equal default-input-method "japanese-katakana"))
;;     (deactivate-input-method))
;;   (when (booleanp current-input-method)
;;     (set-input-method "japanese-hiragana"))
;;   )

;; Packages

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

;; use-package things
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t) ;; make sure we download when necessary
(require 'diminish)
(require 'bind-key)
(require 'cl)
(use-package dash)
(use-package s)

(use-package rainbow-delimiters
  :config
  ;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

;; el-get stuff
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (package-install 'el-get)
  (require 'el-get))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(use-package evil-leader
  :init
  (add-to-list 'load-path (user-emacs-subdirectory "packages/evil-leader"))
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  )

(use-package undo-tree)
(use-package goto-chg)

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)

  :config
  (evil-mode 1)


  (setq evil-want-Y-yank-to-eol t)
  (setq sentence-end-double-space nil)
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
       (define-key evil-normal-state-map (kbd "gt") '(lambda () (interactive) (other-frame 1)))
       (define-key evil-normal-state-map (kbd "gT") '(lambda () (interactive) (other-frame -1)))
       (define-key evil-normal-state-map (kbd "C-\\") '(lambda () (interactive) (toggle-input-method)
                                                         (evil-append 1)))
       (define-key evil-ex-map (kbd "SPC") 'helm-M-x)
       )
    )

  ;; Let _ be considered part of a word
  (defadvice evil-inner-word (around underscore-as-word activate)
    (let ((table (copy-syntax-table (syntax-table))))
      (modify-syntax-entry ?_ "w" table)
      ;; (modify-syntax-entry ?- "w" table)
      (with-syntax-table table ad-do-it)
      )
    )

  ;; remap paste command
  (defun evil-paste-after-from-0 ()
    (interactive)
    (let ((evil-this-register ?0))
      (call-interactively 'evil-paste-after)))
  (define-key evil-visual-state-map "p" 'evil-paste-after-from-0)

  ;; change mode-line color evil state
  (lexical-let ((default-color (cons (face-background 'mode-line)
                                     (face-foreground 'mode-line))))
    (add-hook 'post-command-hook
              (lambda ()
                (let ((color (cond ((minibufferp) default-color)
                                   ((evil-insert-state-p) '("#b58900" . "#ffffff"))
                                   ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                   ((buffer-modified-p)   '("#dc322f" . "#ffffff"))
                                   (t default-color))))
                  (set-face-background 'mode-line (car color))
                  (set-face-foreground 'mode-line (cdr color))))))

  (evil-ex-define-cmd "tabn[ew]"     'make-frame)
  (evil-ex-define-cmd "vsp[lit]"     '(lambda()
                                        (interactive)
                                        (split-window-horizontally)
                                        (other-window 1)
                                        ;; (call-interactively #'helm-find-files)
                                        )
                      )
  (evil-ex-define-cmd "sp[lit]"      '(lambda()
                                        (interactive)
                                        (split-window-vertically)
                                        (other-window 1)
                                        ;; (call-interactively # 'helm-find-files)
                                        )
                      )
  )

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

;; (use-package evil-numbers
;;   :config
;;   (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
;;   (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
;;   )

;; (use-package evil-vimish-fold
;;   :config
;;   (evil-vimish-fold-mode 1)
;;   )

(use-package evil-args)
(use-package evil-lion :config (evil-lion-mode))
(use-package evil-matchit)
(use-package evil-cleverparens)
(use-package evil-commentary)
(use-package evil-replace-with-register)
(use-package evil-text-object-python)
(use-package evil-magit)
(use-package evil-indent-textobject)
(use-package vi-tilde-fringe :config (global-vi-tilde-fringe-mode 1))
(use-package evil-visualstar :config (global-evil-visualstar-mode))
(use-package evil-mc :config (global-evil-mc-mode 1))

;; (use-package evil-tabs
;;   :init
;;   (use-package elscreen)
;;   :config
;;   (global-evil-tabs-mode t)
;;   )

(use-package elpy
  :config
  (elpy-enable); TODO: configure elpy
  )

;; ;; Example el-get-sources
;; (setq el-get-sources
;;       '(:name yasnippet
;;            :type github
;;            :pkgname "joaotavora/yasnippet"
;;            :after (yas-global-mode 1)
;;            )
;;       )

(el-get-bundle golden-ratio
  :url "https://github.com/roman/golden-ratio.el"
  )

(require 'epa-file)
(epa-file-enable)

(use-package yasnippet
  :config
  (yas-global-mode 1)
  )

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  )

(use-package swiper)

(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  )

;; activate helm mode
(use-package helm
  :config
  ;; (global-set-key (kbd "M-x") 'helm-M-x)
  (define-key helm-map (kbd "C-w") 'evil-delete-backward-word)
  ;; (define-key helm-map (kbd "<tab>") 'helm-next-line)
  ;; (define-key helm-map (kbd "<backtab>") 'helm-previous-line)
  (define-key helm-map (kbd "<backtab>") 'helm-select-action)
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)

  (helm-mode 1)
  (helm-mode-fuzzy-match t)
  )

(use-package helm-swoop
  :config
  (define-key helm-swoop-map (kbd "C-w") 'evil-delete-backward-word)
  ;; no annoying under mouse highlights
  (setq helm-swoop-pre-input-function (lambda () nil))
  )
(use-package helm-org-rifle)
(use-package helm-flx
  :config
  (helm-flx-mode 1))

(use-package git-gutter
  :config
  (if (display-graphic-p) (use-package git-gutter-fringe))
  (global-git-gutter-mode 1)
  )

(use-package helm-hunks
  :commands (helm-hunks
             helm-hunks-current-buffer
             helm-hunks-staged
             helm-hunks-staged-current-buffer)
  :config
  (add-hook 'helm-hunks-refresh-hook 'git-gutter+-refresh)
  (helm-hunks--is-preview t)
  )

;;solarized dark theme
(use-package solarized-theme
  :config
  (setq solarized-use-variable-pitch nil
        solarized-scale-org-headlines nil
        solarized-high-contrast-mode-line t) ;;unscrew org layout
  (load-theme 'solarized-dark t)
  )

(use-package powershell)

(use-package beacon
  :config
  (beacon-mode 0)
  )

;; (use-package indent-guide ;; this might be a performance hit
;;   :config
;;   (set-face-background 'indent-guide-face "#073642")
;;   (setq indent-guide-delay 0.0
;;      indent-guide-char " ")
;;   (indent-guide-global-mode)
;;   )

(use-package highlight-indent-guides
  :config
  ;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  ;; character || column || fill
  (setq highlight-indent-guides-method 'column
        highlight-indent-guides-character ?\|)
  )

(use-package projectile
  :config
  (use-package helm-projectile)
  (add-hook 'after-init-hook #'projectile-mode)
  )

(use-package whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode 1)
  )

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  )

(use-package spu
  :defer 5 ;; defer package loading for 5 second
  :config (spu-package-upgrade-daily))

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  )

(use-package tramp-term)
(use-package docker-tramp)

;; (use-package linum-relative)

(use-package which-key
  :config
  ;; (setq which-key-popup-type 'minibuffer)
  (which-key-mode 0)
  )

(use-package groovy-mode)
(use-package php-mode)
(use-package dockerfile-mode)
(use-package markdown-mode)
(use-package minimap)
(use-package multiple-cursors)
(use-package transpose-frame)
(use-package buffer-move)

(use-package neotree)

(use-package google-translate)

(use-package origami
  :init
  :config
  (setq origami-show-fold-header t)
  (global-origami-mode 1)
  ) ;; todo: map z-a, z-r, and z-m to these functions. i want folding dammit

(use-package centered-window-mode
  :config
  (centered-window-mode t)
  )

;; (use-package sublimity
;;   :config
;;   (sublimity-mode 0)
;;   :if (display-graphic-p) (use-package sublimity-attractive)
;;   )

;; (use-package guide-key
;;   :config
;;   (setq guide-key/guide-key-sequence t)
;;   (guide-key-mode 1)
;;   )

(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable emmet's css abbreviation.
  (setq emmet-move-cursor-between-quotes t) ;; default nil
  )

(use-package powerline
  :config
  (use-package powerline-evil
    :config
    (powerline-evil-vim-color-theme)
    )
  (powerline-default-theme)
  )

;; ;; orgmode bindings
;; (use-package org-evil
;;   :config
;;   (setq org-M-RET-may-split-line nil) ;; so we can press 'o' evil and generate the next item
;;   )

(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\Jenkinsfile\\'" . groovy-mode))

;; orgmode config BEGIN
(defun my-evil-org-new-item ()
  (interactive)
  (when (org-in-item-p)
    (end-of-line)
    (org-insert-item)
    (evil-append 1)
    )
  )

(defun my-evil-org-toggle-checkbox ()
  "If the list element has no checkbox, add one. Do nothing otherwise."
  (interactive)
  (if (not (org-at-item-checkbox-p))
      (save-excursion (org-toggle-checkbox '(4))) ;; Prefix arguments are WEIRD
    (org-toggle-checkbox)
    )
  )

(defmacro my-evil-update-cursor-eol(func)
  (lambda ()
    (interactive)
    (func)
    (end-of-line)))

(defun my-evil-org-insert-heading()
  (interactive)
  (org-insert-heading)
  (evil-append-line 1))

;; bind evil normal keymodes inside orgmode
(evil-declare-key 'normal org-mode-map
  (kbd "RET")     'my-evil-org-new-item
  (kbd "M-RET")     'my-evil-org-insert-heading
  (kbd "S-SPC")   'my-evil-org-toggle-checkbox
  (kbd "L")       'org-shiftright
  (kbd "H")       'org-shiftleft
  (kbd "K")       'org-shiftup
  (kbd "J")       'org-shiftdown
  (kbd "M-l")     'org-metaright
  (kbd "M-h")     'org-metaleft
  (kbd "M-k")     'org-metaup
  (kbd "M-j")     'org-metadown
  (kbd "M-L")     '(my-evil-update-cursor-eol(org-shiftmetaright))
  (kbd "M-H")     '(my-evil-update-cursor-eol(org-shiftmetaleft))
  (kbd "M-K")     '(my-evil-update-cursor-eol(org-shiftmetaup))
  (kbd "M-L")     '(my-evil-update-cursor-eol(org-shiftmetadown))
  )

(evil-declare-key 'insert org-mode-map
  (kbd "S-RET")   'my-evil-org-new-item
  (kbd "M-l")     'org-metaright
  (kbd "M-h")     'org-metaleft
  (kbd "M-k")     'org-metaup
  (kbd "M-j")     'org-metadown
  (kbd "M-L")     'org-shiftmetaright
  (kbd "M-H")     'org-shiftmetaleft
  (kbd "M-K")     'org-shiftmetaup
  (kbd "M-L")     'org-shiftmetadown
  )

;; (set-face-attribute 'org-table nil :inherit 'fixed-pitch)

(list ) ;; org agenda list

;; orgmode config END

;; company mode
(use-package company
  :config
  (global-company-mode)

  (use-package company-quickhelp
    :config
    (company-quickhelp-mode 0)
    ;; (setq company-quickhelp-delay 1)
    )

  (use-package company-jedi
    :config
    (defun my/python-mode-hook ()
      (add-to-list 'python-mode-hook 'company-jedi))
    (add-hook 'python-mode-hook 'my/python-mode-hook)
    )

  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  (use-package helm-company
    :config
    (evil-declare-key 'insert company-mode-map (kbd "C-SPC") 'helm-company)
    (evil-declare-key 'insert company-active-map (kbd "C-SPC") 'helm-company)
    )

  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-idle-delay 0
        company-require-match nil
        company-selection-wrap-around t)

  (define-key company-active-map (kbd "C-h") 'company-quickhelp-manual-begin)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)

  ;; (define-key company-active-map (kbd "TAB") 'helm-company)
  (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
  )

(setq custom-file (user-emacs-subdirectory "custom.el"))
(load custom-file)

;; No startup screen
(setq inhibit-startup-screen t)

;; turn on line numbers
(global-linum-mode 0) ;; THIS MIGHT HAVE PERFORMANCE ISSUES

;; set default font
;; (add-to-list 'default-frame-alist '(font . "Consolas-11"))
(set-frame-font "Consolas-11" nil t)

;; startup maximised
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; autopairing
(electric-pair-mode 1)

;; indentation
(electric-indent-mode 1)
(setq-default indent-tabs-mode nil)

;; Remove toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(window-divider-mode -1)

(setq truncate-lines    t
      tab-width         8
      auto-hscroll-mode t)

;; (global-hl-line-mode 1)
;; (set-face-background 'hl-line  "#073642")
;; (set-face-foreground 'highlight nil)

;; Japanese mode
(setq default-input-method "japanese"
      kkc-show-conversion-list-count 1) ;; ばかがいじん

(with-eval-after-load "kkc"
  (define-key kkc-keymap (kbd "SPC") 'kkc-terminate)
  (define-key kkc-keymap (kbd "<tab>") 'kkc-next)
  (define-key kkc-keymap (kbd "<backtab>") 'kkc-prev)
  )

;; しん おれを ワタ

(evil-leader/set-key
  "<SPC>"	'helm-M-x
  "/"		'(lambda ()
                   (interactive)
                   (helm-swoop :$query "" :$multiline 4))
  "\\"          'helm-hunks
  "t"		'(lambda () (interactive)
                   (org-time-stamp '(16) t))
  "cc"          'comment-region
  "cu"          'uncomment-region
  "a"		'evil-lion-left
  "A"		'evil-lion-right
  "."		'centered-window-mode

  ","		'magit-status
  "'"           'highlight-indent-guides-mode
  )

(evil-ex-define-cmd "re[cent]"     'helm-recentf)
(evil-ex-define-cmd "pr[ojectile]" 'helm-projectile)
(evil-ex-define-cmd "or[gsearch]"  'helm-org-rifle)
(evil-ex-define-cmd "goo[gle]"     'helm-google-suggest)
(evil-ex-define-cmd "e[dit]"     'helm-find-files)
(evil-ex-define-cmd "e!"     '(lambda() (interactive) (revert-buffer t t t)))
(evil-ex-define-cmd "b[uffer]"     'helm-mini)
(evil-ex-define-cmd "ini[t]"     'find-user-init-file)
(define-key evil-ex-map "tabe"
  '(lambda() (interactive)
     (make-frame)))

(evil-define-command my-evil-helm-apropos(arg)
  (interactive "<a>")
  (helm-apropos arg)
  (other-window 1)
  )

(eval-after-load 'evil-maps
  '(progn
     ;; (define-key evil-ex-map "b" 'helm-mini)
     ;; (define-key evil-ex-map "e" 'helm-find-files)
     (evil-ex-define-cmd "h[elp]" 'my-evil-helm-apropos)
     ))

;; Overload shifts so that they don't lose the selection
(defun my-evil-shift-left-visual ()
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun my-evil-shift-right-visual ()
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(define-key evil-visual-state-map (kbd ">>") 'my-evil-shift-right-visual)
(define-key evil-visual-state-map (kbd "<<") 'my-evil-shift-left-visual)

;; Save buffer state
(desktop-save-mode 1)
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)

;; Display time
(display-time-mode 1)

;; strip whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Remove really annoying bindings
(global-unset-key (kbd "M-:"))

;; Reduce gc threshold to more manageable values:
(setq gc-cons-threshold default-gc-cons-threshold)

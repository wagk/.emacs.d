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
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/"))
  "Sets up the startup directory.")

(defconst user-config-dir
  (concat user-init-dir "config/"))

(defun load-user-config-file (file)
  "Load FILE as configuration file.
Assumes that it:
- Is a configuration file (i.e. elisp)
- Is relative to user-init-dir"
  (interactive "f")
  (load-file (expand-file-name file user-config-dir)))

;; Add to load path our configuration folder
(add-to-list 'load-path user-config-dir)

;; tweak garbage collector before
(defvar default-gc-cons-threshold 20000000)
(setq gc-cons-threshold most-positive-fixnum)

;; load each config file in order
(load-user-config-file "config-utility.el")
(load-user-config-file "config-package.el")
(load-user-config-file "config-common.el")
(load-user-config-file "config-evil.el")
(load-user-config-file "config-helm.el")
(load-user-config-file "config-snippets.el")
(load-user-config-file "config-theme.el")
(load-user-config-file "config-git.el")

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
  "Checks if the filename is a binary file"
  (with-current-buffer (find-file-noselect filename :no-warn)
    (prog1 (not (eq buffer-file-coding-system 'no-conversion))
      (kill-buffer))))

(use-package rainbow-delimiters
  :config
  ;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )


;; (use-package evil-leader
;;   :init
;;   (add-to-list 'load-path (user-emacs-subdirectory "packages/evil-leader"))
;;   :config
;;   (global-evil-leader-mode)
;;   (evil-leader/set-leader "<SPC>")
;;   )

(use-package undo-tree)
(use-package goto-chg)

;; (use-package evil
;;   :init
;;   (setq evil-want-C-u-scroll t)

;;   :config
;;   (fset 'evil-visual-update-x-selection 'ignore)
;;   (setq evil-want-Y-yank-to-eol t
;;         sentence-end-double-space nil
;;         evil-regexp-search t
;;         evil-normal-state-cursor '(box "red"))
;;   (setq evil-normal-state-modes (append evil-motion-state-modes evil-normal-state-modes))

;;   (setq evil-motion-state-modes nil)
;;   (define-key global-map (kbd "C-f") 'universal-argument)
;;   (define-key universal-argument-map (kbd "C-u") nil)
;;   (define-key universal-argument-map (kbd "C-f") 'universal-argument-more)
;;   (define-key global-map (kbd "C-u") 'kill-whole-line)
;;   (add-hook 'view-mode-hook 'evil-motion-state)
;;   (eval-after-load 'evil-maps
;;     '(progn
;;        (define-key evil-motion-state-map (kbd "C-f") nil)
;;        (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)
;;        (define-key evil-normal-state-map (kbd "gt") '(lambda () (interactive) (other-frame 1)))
;;        (define-key evil-normal-state-map (kbd "gT") '(lambda () (interactive) (other-frame -1)))
;;        (define-key evil-normal-state-map (kbd "C-\\") '(lambda () (interactive) (toggle-input-method)
;;                                                          (evil-append 1)))
;;        (define-key evil-normal-state-map (kbd "<f5>") '(lambda () (interactive) (org-time-stamp '(16) t)))
;;        (define-key evil-insert-state-map (kbd "<f5>") '(lambda () (interactive) (org-time-stamp '(16) t)))
;;        )
;;     )

;;   ;; Let _ be considered part of a word
;;   (defadvice evil-inner-word (around underscore-as-word activate)
;;     (let ((table (copy-syntax-table (syntax-table))))
;;       (modify-syntax-entry ?_ "w" table)
;;       ;; (modify-syntax-entry ?- "w" table)
;;       (with-syntax-table table ad-do-it)
;;       )
;;     )

;;   ;; remap paste command
;;   (defun evil-paste-after-from-0 ()
;;     (interactive)
;;     (let ((evil-this-register ?0))
;; (call-interactively 'evil-paste-after)))
;; (define-key evil-visual-state-map "p" 'evil-paste-after-from-0)

;; ;; change mode-line color evil state
;; (lexical-let ((default-color (cons (face-background 'mode-line)
;;                                (face-foreground 'mode-line))))
;; (add-hook 'post-command-hook
;;         (lambda ()
;;           (let ((color (cond ((minibufferp) default-color)
;;                              ((evil-insert-state-p) '("#b58900" . "#ffffff"))
;;                              ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
;;                              ((buffer-modified-p)   '("#dc322f" . "#ffffff"))
;;                              (t default-color))))
;;             (set-face-background 'mode-line (car color))
;;             (set-face-foreground 'mode-line (cdr color))))))

;; (evil-ex-define-cmd "tabn[ew]"     'make-frame)
;; (evil-ex-define-cmd "vsp[lit]"     '(lambda()
;;                                   (interactive)
;;                                   (split-window-horizontally)
;;                                   (other-window 1)
;;                                   ;; (call-interactively #'helm-find-files)
;;                                   )
;;                 )
;; (evil-ex-define-cmd "sp[lit]"      '(lambda()
;;                                   (interactive)
;;                                   (split-window-vertically)
;;                                   (other-window 1)
;;                                   ;; (call-interactively # 'helm-find-files)
;;                                   )
;;                 )
;; )

;; (use-package evil-surround
;;   :config
;;   (global-evil-surround-mode 1)
;;   (setq-default evil-surround-pairs-alist (cons '(?~ . ("~" . "~"))
;;                                                 evil-surround-pairs-alist)))

;; (use-package evil-args
;;   :config
;;   ;; bind evil-args text jects
;;   (define-key evil-inner-text-objects-map "i" 'evil-inner-arg)
;;   (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
;;   ;; bind evil-forward/backward-args
;;   (define-key evil-normal-state-map "L" 'evil-forward-arg)
;;   (define-key evil-normal-state-map "H" 'evil-backward-arg)
;;   (define-key evil-motion-state-map "L" 'evil-forward-arg)
;;   (define-key evil-motion-state-map "H" 'evil-backward-arg)
;;   ;; bind evil-jump-out-args
;;   (define-key evil-normal-state-map "K" 'evil-jump-out-args)
;;   )

;; (use-package evil-numbers
;;   :config
;;   (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
;;   (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
;;   )

;; (use-package evil-args)
;; (use-package evil-lion :config (evil-lion-mode))
;; (use-package evil-matchit)
;; ;; (use-package evil-paredit
;; ;;   :config (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode))
;; (use-package evil-cleverparens)
;; (use-package evil-commentary)
;; (use-package evil-replace-with-register)
;; (use-package evil-text-object-python)
;; (use-package evil-magit)
;; (use-package evil-indent-textobject)
;; (use-package vi-tilde-fringe :config (global-vi-tilde-fringe-mode 1))
;; (use-package evil-visualstar :config (global-evil-visualstar-mode))
;; (use-package evil-mc :config (global-evil-mc-mode 0))

;; (use-package elpy
;;   :config
;;   (elpy-enable); TODO: configure elpy
;;   )

;; ;; Example el-get-sources
;; (setq el-get-sources
;;       '(:name yasnippet
;;            :type github
;;            :pkgname "joaotavora/yasnippet"
;;            :after (yas-global-mode 1)
;;            )
;;       )

;; (el-get-bundle golden-ratio
;;   :url "https://github.com/roman/golden-ratio.el"
;;   )

;; crypto
(require 'epa-file)
(epa-file-enable)

;; (use-package yasnippet
;;   :config
;;   (yas-global-mode 1)
;;   )

;; (use-package ivy
;;   :config
;;   (setq ivy-use-virtual-buffers t
;;         enable-recursive-minibuffers t))

;; (use-package swiper)

;; (use-package counsel
;;   :config
;;   (global-set-key (kbd "M-x") 'counsel-M-x))

;; activate helm mode
;; (use-package helm
;;   :config
;;   (define-key helm-map (kbd "C-w") 'evil-delete-backward-word)
;;   (define-key helm-map (kbd "S-SPC") 'helm-select-action)
;;   (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
;;   (helm-mode 1)
;;   (setq helm-recentf-fuzzy-match t
;;         helm-locate-fuzzy-match nil ;; locate fuzzy is worthless
;;         helm-M-x-fuzzy-match t
;;         helm-buffers-fuzzy-matching t
;;         helm-semantic-fuzzy-match t
;;         helm-apropos-fuzzy-match t
;;         helm-imenu-fuzzy-match t
;;         helm-lisp-fuzzy-completion t
;;         helm-completion-in-region-fuzzy-match t)
;;   (setq helm-split-window-in-side-p t)
;;   (progn (helm-autoresize-mode)
;;          (setq helm-autoresize-min-height 40 ;; These numbers are percentages
;;                helm-autoresize-max-height 40)
;;          )
;;   )

(use-package helm-emmet)
;; (use-package helm-describe-modes
;;   :bind ("C-h m" . helm-describe-modes))

;; (use-package helm-descbinds :config (helm-descbinds-mode))

;; (use-package helm-swoop
;;   :config
;;   (define-key helm-swoop-map (kbd "C-w") 'evil-delete-backward-word)
;;   ;; no annoying under mouse highlights
;;   (setq helm-swoop-pre-input-function (lambda () nil))
;;   )
;; (use-package helm-fuzzier :config (helm-fuzzier-mode 1))
;; (use-package helm-flx :config (helm-flx-mode 1))

;; (use-package git-gutter+
;;   :config
;;   (use-package git-gutter-fringe+)
;;   (global-git-gutter+-mode 1)
;;   )

(use-package helm-hunks
  :commands (helm-hunks
             helm-hunks-current-buffer
             helm-hunks-staged
             helm-hunks-staged-current-buffer)
  :config
  (add-hook 'helm-hunks-refresh-hook 'git-gutter+-refresh)
  (setq helm-hunks-preview-diffs t)
  )

;; ;;solarized dark theme
;; (if (display-graphic-p)
;;     (progn (use-package solarized-theme
;;              :config
;;              (setq solarized-use-variable-pitch nil
;;                    solarized-scale-org-headlines nil
;;                    solarized-high-contrast-mode-line t) ;;unscrew org layout
;;              (load-theme 'solarized-dark t)
;;              )
;;            (load-theme 'solarized-dark t)
;;            ))

(use-package powershell)

;; (use-package beacon
;;   :config
;;   (beacon-mode 0)
;;   )

(use-package highlight-indent-guides
  :config
  ;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  ;; character || column || fill
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|)
  )

(if (bound-and-true-p highlight-indentation-mode)
    (highlight-indentation-mode))

;; (progn
;;   (require 'zone)
;;   (zone-when-idle 600)

;;   ;; TODO(pangt): figure out what this does
;;   (defun zone-choose (pgm)
;;     "Choose a PGM to run for `zone'."
;;     (interactive
;;      (list
;;       (completing-read
;;        "Program: "
;;        (mapcar 'symbol-name zone-programs))))
;;     (let ((zone-programs (list (intern pgm))))
;;       (zone)))

;;   (defun zone-pgm-md5 ()
;;     "MD5 the buffer, then recursively checksum each hash."
;;     (let ((prev-md5 (buffer-substring-no-properties ;; Initialize.
;;                      (point-min) (point-max))))
;;       ;; Whitespace-fill the window.
;;       (zone-fill-out-screen (window-width) (window-height))
;;       (random t)
;;       (goto-char (point-min))
;;       (while (not (input-pending-p))
;;         (when (eobp)
;;           (goto-char (point-min)))
;;         (while (not (eobp))
;;           (delete-region (point) (line-end-position))
;;           (let ((next-md5 (md5 prev-md5)))
;;             (insert next-md5)
;;             (setq prev-md5 next-md5))
;;           (forward-line 1)
;;           (zone-park/sit-for (point-min) 0.1)))))

;;   (eval-after-load "zone"
;;     '(unless (memq 'zone-pgm-md5 (append zone-programs nil))
;;        (setq zone-programs
;;              (vconcat zone-programs [zone-pgm-md5]))))
;;   )


(use-package projectile
  :config
  (projectile-mode)
  (add-hook 'after-init-hook #'projectile-mode)
  ;; TODO(pangt): Figure out a way to get this function to ignore binary files
  ;; (defun projectile-whitespace-cleanup-project-files()
  ;;   "Run whitespace-cleanup on all project files"
  ;;   (interactive)
  ;;   (dolist (file (projectile-current-project-files))
  ;;     (let ((path (concat (projectile-project-root) file)))
  ;;       (when (and (find-file-noselect path) (text-file-p path))
  ;;         (let ((buffer (find-file-noselect path)))
  ;;           (when buffer
  ;;             (with-current-buffer buffer
  ;;               (whitespace-cleanup)
  ;;               (save-buffer)
  ;;               (kill-buffer)))
  ;;           ))
  ;;       )))

  ;; (defun projectile-whitespace-cleanup-project-files()
  ;;   "Run whitespace-cleanup on all project files"
  ;;   (interactive)
  ;;   (dolist (file (projectile-current-project-files))
  ;;     (let ((path (concat (projectile-project-root) file))
  ;;           (buffer (find-file-noselect path)))
  ;;       (when (and buffer (text-file-p path))
  ;;         (with-current-buffer buffer
  ;;           (whitespace-cleanup)
  ;;           (save-buffer)
  ;;           (kill-buffer)))
  ;;       )))

  )

(use-package helm-projectile)
;; perspective messes with a lot of things. Helm mini esp
;;(use-package perspective
;;  :config
;;  (persp-mode))
;;(use-package persp-projectile)

(use-package org-projectile
  :config
  (org-projectile:per-repo)
  (setq org-projectile:per-repo-filename ".todo.org"
        org-agenda-files (append org-agenda-files (org-projectile:todo-files))
        )
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

(require 'tramp)
(cond ((eq system-type "windows-nt") (progn (setq tramp-default-method "plink"))))

(use-package tramp-term)

(use-package docker-tramp)

;; (use-package linum-relative)

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

(use-package typescript-mode)
(use-package tide)
(use-package groovy-mode)
(use-package php-mode)
(use-package dockerfile-mode)
(use-package json-mode)
(use-package markdown-mode)
(use-package minimap)
(use-package multiple-cursors)
(use-package transpose-frame)
(use-package buffer-move)
(use-package unicode-troll-stopper)

(use-package neotree)

(use-package google-translate)

(use-package origami
  :config
  (setq origami-show-fold-header t)
  (global-origami-mode 1)
  ) ;; todo: map z-a, z-r, and z-m to these functions. i want folding dammit

;; (use-package centered-window-mode
;;   :config
;;   (centered-window-mode t)
;;   )

(use-package emmet-mode
  :bind (:map emmet-mode-keymap
              ("<tab>" . emmet-next-edit-point)
              ("<backtab>" . emmet-prev-edit-point))
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

(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\Jenkinsfile\\'" . groovy-mode))

;; orgmode config BEGIN
(require 'org)
(org-toggle-link-display) ;;expand link displays
; (use-package helm-org-rifle)
(defun my-evil-org-new-item ()
  "Insert a new item if we're in normal mode."
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
(evil-declare-key   'normal org-mode-map
  (kbd "RET")       'my-evil-org-new-item
  (kbd "M-RET")     'my-evil-org-insert-heading
  (kbd "S-SPC")     'my-evil-org-toggle-checkbox
  (kbd "L")         'org-shiftright
  (kbd "H")         'org-shiftleft
  (kbd "K")         'org-shiftup
  (kbd "J")         'org-shiftdown
  (kbd "M-l")       'org-metaright
  (kbd "M-h")       'org-metaleft
  (kbd "M-k")       'org-metaup
  (kbd "M-j")       'org-metadown
  (kbd "M-L")       '(my-evil-update-cursor-eol(org-shiftmetaright))
  (kbd "M-H")       '(my-evil-update-cursor-eol(org-shiftmetaleft))
  (kbd "M-K")       '(my-evil-update-cursor-eol(org-shiftmetaup))
  (kbd "M-L")       '(my-evil-update-cursor-eol(org-shiftmetadown))
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

;; orgmode config END

;; company mode
(use-package company
  :config
  (global-company-mode)

  (use-package company-quickhelp
    :config
    (company-quickhelp-mode 0)
    (setq company-quickhelp-delay 1)
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
        company-idle-delay 1
        company-require-match nil
        company-selection-wrap-around t)

  (define-key company-active-map (kbd "C-h") 'company-quickhelp-manual-begin)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)

  ;; (define-key company-active-map (kbd "TAB") 'helm-company)
  (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
  )

(setq custom-file (user-emacs-subdirectory "custom.el"))
; (load custom-file)

;; No startup screen
(setq inhibit-startup-screen t)

;; turn on line numbers
(global-linum-mode 0) ;; THIS MIGHT HAVE PERFORMANCE ISSUES

;; set default font
;; (add-to-list 'default-frame-alist '(font . "Consolas-11"))
;; (if (eq system-type "windows-nt")
;;     (set-frame-font "Consolas-11" nil t)
;;   (message "Not windows, not using consolas")
;;   )

;;this should make it global. Solve japanese fonting separately
;; (set-default-font "Courier" nil t )
(set-face-attribute 'default nil :font "Courier-11" )
(set-frame-font "Courier-11" nil t)

;; Switch to cygwin if it exists
;; this isn't working though
;; (when (and (string-equal system-type "windows-nt")
;;            (file-directory-p "C:\cygwin64"))
;;   (setq shell-file-name "C:\cygwin64\bin\bash.exe")
;;   (setq explicit-shell-file-name shell-file-name)
;;   (let (path (getenv "PATH"))
;;     (setq path (replace-regexp-in-string "\\([A-Za-z]\\):" "/\\1" path)
;;           path (replace-regexp-in-string "\\\\" "/" path)
;;           path (replace-regexp-in-string " " "\\\\ " path)
;;           path (concat "~/bin:/usr/local/bin:/usr/bin:" path))
;;     (setenv "PATH" path)
;;     )
;;   )

(let ((msys-path "C:/msys/1.0/bin"))
  (when (and (string-equal system-type "windows-nt")
             (file-directory-p msys-path))
    (setq exec-path (append exec-path '(msys-path)))
    )
  )

;; startup maximised
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; autopairing
(electric-pair-mode 1)

;; indentation
(electric-indent-mode 1)
(setq-default indent-tabs-mode nil)

;; Remove toolbar
(progn
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (window-divider-mode -1))

(setq truncate-lines    t
      tab-width         8
      auto-hscroll-mode t)

;; (global-hl-line-mode 1)
;; (set-face-background 'hl-line  "#073642")
;; (set-face-foreground 'highlight nil)

;; Japanese mode
(progn (setq default-input-method "japanese"
             kkc-show-conversion-list-count 1)
       (with-eval-after-load "kkc"
         (define-key kkc-keymap (kbd "SPC")       'kkc-terminate)
         (define-key kkc-keymap (kbd "<tab>")     'kkc-next)
         (define-key kkc-keymap (kbd "<backtab>") 'kkc-prev)
         )
       )

;; しん おれを ワ

(use-package misc-cmds)
(defun my-line-lengths()
  (let (length)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (push (- (line-end-position) (line-beginning-position))
              length)
        (forward-line)))
    (copy-sequence length) ;; we return a list since this is the last form evaluated
    ))

(defun my-longest-line-length()
  (let ((lines (my-line-lengths)))
    (nth 0 (sort lines '>)) ;; return the first element, which should be the largest
    ))

(defun my-centre-window-function()
  (interactive)
  (let ((margin-size (/ (- (window-width) (my-longest-line-length) ) 2)))
    (if (not (get 'my-centre-window-function 'active))
        (progn
          (set-window-margins nil margin-size margin-size)
          (put 'my-centre-window-function 'active t)
          )
      (progn
        (set-window-margins nil 0 nil)
        (put 'my-centre-window-function 'active nil)
        ))))

(progn
  (evil-leader/set-key
    ;; "<SPC>"    'helm-M-x
    ;; "S-<SPC>"  'helm-resume
    ;; "y"        'helm-show-kill-ring
    "/"        '(lambda () (interactive) (helm-swoop :$query "" :$multiline 4))
    "?"        '(lambda () (interactive) (helm-swoop :$query "" :$multiline 4))
    "."        'helm-hunks-current-buffer
    "t"        '(lambda () (interactive) (org-time-stamp '(16) t))
    "cc"       'comment-region
    "cu"       'uncomment-region
    "a"        'evil-lion-left
    "A"        'evil-lion-right
    ";"        'my-centre-window-function
    "\\"       'org-capture
    "]"        'org-projectile:template-or-project
    ","        'magit-status
    "'"        'highlight-indent-guides-mode
    ;; "h"        'helm-apropos
    ;; "h"     '(lambda () (interactive) (helm-apropos nil)
    ;;            (switch-to-buffer-other-window "*Help*"))
    ;; "-"        'helm-find-files
    ;; "_"        'helm-mini
    ;; command to go to last buffet in vim is <C-^> and <C-6>
    ;; "b"        'helm-bookmarks
    "w"        'helm-projectile
    )

  (evil-ex-define-cmd "sh[ell]"       'shell)
  (evil-ex-define-cmd "re[cent]"      'helm-recentf)
  (evil-ex-define-cmd "pr[ojectile]"  'helm-projectile)
                                        ; (evil-ex-define-cmd "or[gsearch]"   'helm-org-rifle)
  (evil-ex-define-cmd "goo[gle]"      'helm-google-suggest)
  (evil-ex-define-cmd "e[dit]"        'helm-find-files)
  (evil-ex-define-cmd "e!"            '(lambda() (interactive)
                                         (revert-buffer t t t)))
  (evil-ex-define-cmd "b[uffer]"      'helm-mini)
  (evil-ex-define-cmd "ini[t]"        'find-user-init-file)
  (evil-ex-define-cmd "todo"          '(lambda() (interactive)
                                         (insert "TODO(pangt): ")
                                         (comment-region (line-beginning-position) (line-end-position))
                                         (indent-relative)
                                         (end-of-line)
                                         (evil-insert nil)))
  ;; TODO(pangt): we fix this some day
  (evil-define-command my-evil-tabedit(arg) (interactive "<a>")
    ;; (generate-buffer-create arg)
    ;; (make-frame ((buffer-list . arg)))
    (make-frame)
    )

  (evil-ex-define-cmd "tabe[dit]"     'my-evil-tabedit)

  (evil-define-command my-evil-helm-apropos(arg)
    (interactive "<a>")
    (helm-apropos arg)
    (other-window 1)
    )

  (eval-after-load 'evil-maps '(progn (evil-ex-define-cmd "h[elp]" 'my-evil-helm-apropos)))

  ;; Overload shifts so that they don't lose the selection
  (defun my-evil-shift-left-visual ()
    "Keep visual selection after shifting left."
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun my-evil-shift-right-visual ()
    "Same as my-evil-shift-left-visual, but for the right instead."
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (define-key evil-visual-state-map (kbd ">>") 'my-evil-shift-right-visual)
  (define-key evil-visual-state-map (kbd "<<") 'my-evil-shift-left-visual)
  )

(setq require-final-newline t)

;; remove annoying bell sound
(setq ring-bell-function 'ignore)

;; Save buffer state
;; (desktop-save-mode 1)
(setq history-length 250)
;; (add-to-list 'desktop-globals-to-save 'file-name-history)

;; Display time
(display-time-mode 1)

;; strip whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Remove really annoying bindings
;; (global-set-key (kbd "ESC") 'evil-force-normal-state)
(global-set-key (kbd "M-:") nil)
(global-set-key (kbd "M-ESC :") nil)

;; make sure backspace works on terminals
(unless (display-graphic-p)
  (define-key key-translation-map [?\C-h] [?\C-?]))
;; Reduce gc threshold to more manageable values:
(setq gc-cons-threshold default-gc-cons-threshold)
; (evil-mode 1)

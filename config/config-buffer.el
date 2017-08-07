;;; config-buffer.el --- Buffer visual configuration

;;; Commentary:

;;; Code:

(require 'config-package)
(require 'config-evil)
(require 'config-common)

(use-package color-theme
  :ensure t)

(use-package solarized-theme
  :ensure t
  :config
  (setq  solarized-use-variable-pitch nil
         solarized-distinct-fringe-background t
         solarized-high-contrast-mode-line t))

(use-package base16-theme
  :disabled
  :if (not (display-graphic-p))
  :ensure t
  :config
  (load-theme 'base16-solarized-dark t))

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|)
  (progn (require 'evil-leader)
         (evil-leader/set-key
           "'" 'highlight-indent-guides-mode))
  (highlight-indent-guides-mode)
  )

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish t
  :config
  (global-whitespace-cleanup-mode 1))

(use-package hl-todo
  :ensure t
  :config
  (add-to-list 'hl-todo-keyword-faces '("BUG" . "#d0bf8f"))
  (add-to-list 'hl-todo-keyword-faces '("DEBUG" . "#d0bf8f"))
  (global-hl-todo-mode))

;; https://github.com/alpaker/Fill-Column-Indicator
(use-package fill-column-indicator
  :ensure t
  :diminish t
  :config
  (setq-default fill-column 80)
  ;; (setq fci-rule-width 23)
  (add-hook 'prog-mode-hook 'turn-on-fci-mode))

(use-package powerline
  :ensure t
  :config
  (powerline-vim-theme))

(use-package multi-term
  :ensure t
  :config
  (cond ((or (eq system-type 'ms-dos)
             (eq system-type 'windows-nt)) (setq multi-term-program "cmd"))
        (t (setq multi-term-program "/bin/bash")))
  (progn (require 'evil)
         (evil-ex-define-cmd "te[rminal]" 'multi-term)))

(use-package unicode-troll-stopper
  :ensure t)

(use-package transpose-frame
  :ensure t)

(use-package buffer-move
  :ensure t)

(defun /line-lengths()
  (let (length)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (push (- (line-end-position)
                 (line-beginning-position))
              length)
        (forward-line)))
    ;; we return a list since this is the last form evaluated
    (copy-sequence length)))

(defun /longest-line-length()
  (let ((lines (/line-lengths)))
    ;; return the first element, which should be the largest
    (nth 0 (sort lines '>))))

(defun /centre-window-function()
  ""
  (interactive)
  (let ((margin-size (/ (abs (- (window-width) (/longest-line-length))) 2)))
    (if (not (get '/centre-window-function 'active))
        (progn
          (set-window-margins nil margin-size margin-size)
          (fringe-mode '(1 . 1))
          (put '/centre-window-function 'active t))
      (progn
        (set-window-margins nil nil nil)
        (fringe-mode nil)
        (put '/centre-window-function 'active nil)))))

(evil-leader/set-key
  ";" '/centre-window-function)

;; Set transparency of emacs
(defun /set-frame-transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(add-hook 'prog-mode-hook 'hs-minor-mode)

(load-theme 'solarized-dark t)

;; no startup screen
(setq inhibit-startup-screen t)

;; startup maximised
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

(setq require-final-newline t)

;; remove annoying bell sound
(setq ring-bell-function 'ignore)

;; Save buffer state
(setq history-length 250)

;; Display time
(display-time-mode 1)

;; strip whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(progn (require 'evil-leader)
       (evil-leader/set-key
         "." 'whitespace-mode))

;; automatically refresh buffer when changed outside
(global-auto-revert-mode t)

;; Remove toolbar
(progn (tool-bar-mode -1)
       (menu-bar-mode -1)
       (scroll-bar-mode -1)
       (window-divider-mode -1))

(setq tab-always-indent 'complete)

(setq-default truncate-lines    t  ;; no wrap
              indent-tabs-mode nil ;; do not use tabs when indenting
              tab-width         2
              auto-hscroll-mode t)

;; autopairing
(electric-pair-mode 1)

;; Change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; Frame-related functions
(add-hook 'after-make-frame-functions 'select-frame)

;; adjust autosave and backup directories
(setq backup-directory-alist `(("." . ,(concat user-init-dir "/backups/")))
      delete-old-versions -1
      version-control t
      vc-make-backup-files t
      auto-save-file-name-transforms `((".*" ,(concat user-init-dir "/autosave/") t)))

(provide 'config-buffer)

;;; config-buffer.el ends here

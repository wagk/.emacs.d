;;; config-buffer.el --- Buffer visual configuration

;;; Commentary:

;;; Code:

(require 'config-package)
(require 'config-evil)
(require 'config-common)
(require 'config-colors)


(add-hook 'prog-mode-hook 'hs-minor-mode)

;; no startup screen
(setq inhibit-startup-screen t)

;; startup maximised
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
(custom-set-variables
 '(default-frame-alist (add-to-list 'default-frame-alist
                                    '(fullscreen . maximized))))

(setq require-final-newline t)

;; remove annoying bell sounds
(setq ring-bell-function 'ignore)

;; Save buffer state
(setq savehist-file (concat user-init-dir "history")
      savehist-save-minibuffer-history 1
      savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))
(savehist-mode 1)
(setq history-length t
      history-delete-duplicates t)


;; Display time
(display-time-mode 1)

;; strip whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(general-define-key :prefix my-default-evil-leader-key
                    "." 'whitespace-mode)

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

;; buffer encoding systems
(prefer-coding-system 'utf-8)

;; use optimised linum mode if we can
(when (>= emacs-major-version 26)
  (global-display-line-numbers-mode))

(defun my-disable-line-numbers ()
  "For modes that doesn't need line numbers in their buffers"
  (display-line-numbers-mode -1)
  )

;; autopairing
;; We're currently trying out smartparens
(electric-pair-mode -1)

;; Change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; Frame-related functions
(add-hook 'after-make-frame-functions 'select-frame)

;; speed optimisation
;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq-default auto-window-vscroll nil)

;; adjust autosave and backup directories
(setq backup-directory-alist `(("." . ,(concat user-init-dir "/backups/")))
      delete-old-versions -1
      version-control t
      vc-make-backup-files t
      auto-save-file-name-transforms `((".*" ,(concat user-init-dir "/autosave/") t)))

(defun my-goto-scratch-buffer ()
  "When called goes to the scratch buffer.
TODO: Make it take an argument that specifies which mode it should enter the
buffer in."
  (interactive)
  (switch-to-buffer "*scratch*")
  )

(evil-ex-define-cmd "sc[ratch]" 'my-goto-scratch-buffer)

(defun my-goto-messages-buffer ()
  "When called goes to the Messages buffer.
TODO: Make it take an argument that specifies which mode it should enter the
buffer in."
  (interactive)
  (switch-to-buffer "*Messages*")
  )

(evil-ex-define-cmd "me[ssages]" 'my-goto-messages-buffer)

(use-package highlight-indent-guides
  ;; :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (general-define-key :prefix my-default-evil-leader-key
                      "'" 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|)
  ;; (highlight-indent-guides-mode)
  )

(use-package whitespace-cleanup-mode
  :demand t
  :config
  (global-whitespace-cleanup-mode 1))

(use-package hl-todo
  :defer 1
  :diminish t
  :init
  (general-define-key :prefix my-default-evil-leader-key
                      "t t" 'hl-todo-occur)
  ;; :hook (prog-mode . hl-todo-mode)
  :bind (:map evil-normal-state-map
              ("[ t" . hl-todo-previous)
              ("] t" . hl-todo-next))
  :config
  (customize-set-variable 'hl-todo-keyword-faces
                          `(("TODO"  . ,$solarized-dark-yellow)
                            ("DEBUG" . ,$solarized-dark-magenta)
                            ("BUG"   . ,$solarized-dark-red)
                            ("IMPL"  . ,$solarized-dark-green)
                            ("NOTE"  . ,$solarized-dark-base1)
                            ("HACK"  . ,$solarized-dark-violet)
                            ("FIXME" . ,$solarized-dark-orange)))
  (global-hl-todo-mode)
  (add-hook 'yaml-mode-hook 'hl-todo-mode))

;; https://github.com/alpaker/Fill-Column-Indicator
(use-package fill-column-indicator
  :hook (prog-mode . turn-on-fci-mode)
  :diminish t
  :config
  (setq-default fill-column 80)
  ;; (setq fci-rule-width 23)
  )

(use-package golden-ratio
  :disabled t
  :config
  (golden-ratio-mode 1)
  (add-hook 'buffer-list-update-hook #'golden-ratio))

(use-package powerline
  :demand t
  ;; :config
  ;; (powerline-vim-theme)
  )

(use-package powerline-evil
  :after powerline
  :demand t
  :config
  (setq powerline-evil-tag-style 'verbose)
  (powerline-evil-vim-theme)
  )

;; https://github.com/larstvei/Focus
(use-package focus
  :init
  (general-define-key :prefix my-default-evil-leader-key
                      "f f" 'focus-mode)
  (evil-ex-define-cmd "fo[cus]" 'focus-mode))

(use-package minimap
  :commands minimap-mode
  :config
  (customize-set-variable 'minimap-window-location 'right))

(use-package no-littering)

(use-package mmm-mode
  :disabled
  :commands mmm-mode
  :config
  (setq mmm-parse-when-idle 't))

(use-package unicode-troll-stopper)

(use-package transpose-frame)

(use-package buffer-move)

(use-package crosshairs)

(use-package which-key
  :demand t
  :config
  (which-key-mode))

;;;###autoload
(defun /line-lengths()
  "Return a list of line lengths for all the lines in the buffer."
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

;;;###autoload
(defun /longest-line-length()
  "Return the longest line from the list of lines given."
  (let ((lines (/line-lengths)))
    ;; return the first element, which should be the largest
    (nth 0 (sort lines '>))))

;;;###autoload
(defun /centre-window-function()
  "Offset the window margins based on the longest line in the buffer.
This effectively centers it."
  (interactive)
  (let ((margin-size (/ (abs (- (window-width) (/longest-line-length))) 2)))
    (if (not (get '/centre-window-function 'active))
        (progn
          (set-window-margins nil margin-size nil)
          (fringe-mode '(1 . 1))
          (put '/centre-window-function 'active t))
      (progn
        (set-window-margins nil nil nil)
        (fringe-mode nil)
        (put '/centre-window-function 'active nil)))))

(general-define-key :prefix my-default-evil-leader-key
                    "W" '/centre-window-function)

(use-package centered-window-mode
  :disabled t
  :el-get centered-window-mode
  :config
  (centered-window-mode t))

(use-package autopair
  :disabled t
  :config
  (autopair-global-mode))

(use-package polymode)

(use-package smartparens
  :demand t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config) ;; load some default configurations
  (smartparens-global-mode)
  (show-smartparens-global-mode t)
  ;; define some helper functions
  (defun my-add-newline-and-indent-braces (&rest _)
    "Adds that cool vim indent thing we always wanted"
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  ;; Update the global definitions with some indenting
  ;; I think that the nil is the flag that controls property inheritance
  ;;NOTE: For some reason TAB isn't recognised. Might be yasnippet intefering.
  ;;Learn to use ret for now
  (sp-pair "{" nil :post-handlers '((my-add-newline-and-indent-braces "RET")))
  (sp-pair "[" nil :post-handlers '((my-add-newline-and-indent-braces "RET")))
  (sp-pair "(" nil :post-handlers '((my-add-newline-and-indent-braces "RET")))
  )

;;;###autoload
(defun my-set-frame-transparency (value)
  "Set the transparency of the frame window to VALUE.
0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(provide 'config-buffer)

;;; config-buffer.el ends here

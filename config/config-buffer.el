;;; config-buffer.el --- Buffer visual configuration

;;; Commentary:

;;; Code:

(require 'config-package)
(require 'config-evil)

(use-package color-theme
  :ensure t)

(use-package solarized-theme
  ;; :if (display-graphic-p)
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
  :after evil-leader
  :config
  ;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  ;; character || column || fill
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|)
  (evil-leader/set-key
    "'" 'highlight-indent-guides-mode))

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish t
  :config
  (global-whitespace-cleanup-mode 1))

(use-package aggressive-indent
  :ensure t
  :diminish t
  :config
  (global-aggressive-indent-mode 1))

;; https://github.com/alpaker/Fill-Column-Indicator
(use-package fill-column-indicator
  :ensure t
  :diminish t
  :config
  (setq-default fill-column 80)
  (add-hook 'prog-mode-hook 'turn-on-fci-mode))

(use-package powerline
  :ensure t
  :config
  ;; (use-package powerline-evil
  ;;   :ensure t
  ;;   :config
  ;;   (powerline-evil-vim-color-theme))
  (powerline-vim-theme))

(load-theme 'solarized-dark t)

;; no startup screen
(setq inhibit-startup-screen t)

;; set font
;; (set-face-attribute 'default nil :font "Courier-11" )
;; (set-frame-font "Courier-11" nil t)

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

;; adjust autosave and backup directories
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(provide 'config-buffer)

;;; config-buffer.el ends here

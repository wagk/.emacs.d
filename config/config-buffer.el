;;; config-buffer.el --- Buffer visual configuration

;;; Commentary:

;;; Code:

(require 'config-package)

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
  :config
  ;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  ;; character || column || fill
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|))

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
  (fci-mode 1))

(use-package powerline
  :ensure t
  :config
  (use-package powerline-evil
    :ensure t
    :config
    (powerline-evil-vim-color-theme))
  (powerline-default-theme))

;; (if (display-graphic-p)
;;     (load-theme 'solarized-dark t)
;;   (load-theme 'solarized t))

(load-theme 'solarized-dark t)

(provide 'config-buffer)

;;; config-buffer.el ends here

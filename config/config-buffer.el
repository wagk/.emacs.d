;;; config-buffer.el --- Buffer visual configuration

;;; Commentary:

;;; Code:

(require 'config-package)

(use-package solarized-theme
  :ensure t
  :config
  (setq  solarized-use-variable-pitch nil
         ;; solarized-scale-org-headlines nil
         solarized-high-contrast-mode-line t))

(use-package highlight-indent-guides
  :ensure t
  :config
  ;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  ;; character || column || fill
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|)
  )

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

(load-theme 'solarized-dark t)

(provide config-buffer)

;;; config-buffer.el ends here

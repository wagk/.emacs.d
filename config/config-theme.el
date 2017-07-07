;;; config-theme --- Colorschemes and themes

;;; Commentary:

;;; Code:

(require 'config-package)
(require 'config-common)

(use-package solarized-theme
  :ensure t
  :config
  (setq  solarized-use-variable-pitch nil
         ;; solarized-scale-org-headlines nil
         solarized-high-contrast-mode-line t))

(load-theme 'solarized-dark t)

(provide 'config-theme)

;;; config-theme.el ends here

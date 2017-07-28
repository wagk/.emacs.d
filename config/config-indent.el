;;; config-indent.el --- indentation modes

;;; Commentary:

;;; Code:
(require 'config-package)

(use-package aggressive-indent
  :ensure t
  :diminish t
  :config
  (global-aggressive-indent-mode 1))

(use-package aggressive-fill-paragraph
  ;; :disabled ;; this package annoys me. Probably needs more config
  :ensure t
  :commands (aggressive-fill-paragraph-mode
             afp-setup-recommended-hooks)
  :config
  ;; (add-hook 'text-mode-hook #'aggressive-fill-paragraph-mode)
  )

(provide 'config-indent)

;;; config-indent.el ends here

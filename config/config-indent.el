;;; config-indent.el --- indentation modes

;;; Commentary:

;;; Code:
(require 'config-package)

(use-package aggressive-indent
  :diminish t
  :demand t
  :config
  (add-hook 'python-mode-hook 'aggressive-indent-mode)
  ;; (global-aggressive-indent-mode)
  )

(use-package aggressive-fill-paragraph
  ;; :disabled ;; this package annoys me. Probably needs more config
  :commands (aggressive-fill-paragraph-mode
             afp-setup-recommended-hooks)
  ;; :config
  ;; (add-hook 'text-mode-hook #'aggressive-fill-paragraph-mode)
  )

(provide 'config-indent)

;;; config-indent.el ends here

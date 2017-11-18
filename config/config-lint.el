;;; config-lint.el --- linting plugins

;;; Commentary:

;;; Code:
(require 'config-package)

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

(provide 'config-lint)

;;; config-lint.el ends here

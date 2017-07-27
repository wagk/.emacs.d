;;; config-typescript.el --- Typescript-specific configuration

;;; Commentary:

;;; Code:
(require 'config-package)

(use-package typescript-mode
  :ensure t)

(use-package tide
  :ensure t
  :after typescript-mode
  :config
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook 'tide-setup))


(provide 'config-typescript)

;;; config-typescript.el ends here

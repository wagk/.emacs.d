;;; config-typescript.el --- Typescript-specific configuration

;;; Commentary:

;;; Code:
(require 'config-package)

(use-package typescript-mode
  :mode ("\\.tsx\\'" . typescript-mode)
  )

(use-package tide
  :after typescript-mode
  :init
  (add-hook 'typescript-mode-hook 'tide-setup)
  :config
  (add-hook 'before-save-hook 'tide-format-before-save)
  )

(provide 'config-typescript)

;;; config-typescript.el ends here

;;; config-visualise.el --- pdfs and images and all that

;;; Commentary:

;;; Code:
(require 'config-package)

(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(provide 'config-visualise)

;;; config-visualise.el ends here

;;; config-cpp.el --- c/cpp/c#/c-family configurations

;;; Commentary:

;;; Code:
(require 'config-package)

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(provide 'config-cpp)

;;; config-cpp.el ends here

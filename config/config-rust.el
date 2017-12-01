;;; config-rust.el --- rust lang related configs

;;; Commentary:

;;; Code:
(require 'config-package)

(use-package rust-mode
  :mode ("\\.rs\\" . rust-mode))

(provide 'config-rust)

;;; config-rust.el ends here

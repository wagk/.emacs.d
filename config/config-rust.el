;;; config-rust.el --- rust lang related configs

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (general-define-key :states 'insert
                      :keymaps 'rust-mode-map
                      "RET" 'comment-indent-new-line))

(provide 'config-rust)

;;; config-rust.el ends here

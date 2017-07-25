;;; config-prog.el --- umbrella program package configuration file

;;; Commentary:

;;; Code:
(require 'config-package)

(use-package ctags-update
  :ensure t
  :config
  (autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on 'ctags-auto-update-mode'." t))

(provide 'config-prog)

;;; config-prog.el ends here

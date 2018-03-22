;;; config-fs.el --- filesystem configs

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)

(general-define-key
 :states 'normal
 :keymaps 'dired-mode-map
  "<SPC>" nil ; was shadowing leader key bindings
  "C-l" 'dired-up-directory)

;;TODO: Make bind this to work like netrw or something


(provide 'config-fs)

;;; config-fs.el ends here

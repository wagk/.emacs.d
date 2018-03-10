;;; config-fs.el --- filesystem configs

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)

(define-key dired-mode-map (kbd "C-l") 'dired-up-directory)
(general-define-key :keymaps 'dired-mode-map
                    "<SPC>" nil) ;; was shadowing leader key bindings

;;TODO: Make bind this to work like netrw or something


(provide 'config-fs)

;;; config-fs.el ends here

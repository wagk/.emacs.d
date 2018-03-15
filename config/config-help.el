;;; config-help.el --- additional help functions

;;; Commentary:

;;; Code:

(require 'config-package)
(require 'config-evil)

(use-package help+
  :load-path "local-packages/"
  :demand t)

(use-package help-macro+
  :load-path "local-packages/"
  :demand t)

(use-package help-mode+
  :load-path "local-packages/"
  :demand t)

(use-package help-fns+
  :load-path "local-packages/"
  :demand t)

(general-define-key :states 'normal
                    :keymaps 'help-mode-map
                    "C-t" 'help-go-back
                    "C-]" 'help-follow)

(general-define-key :states 'normal
                    :keymaps 'Info-mode-map
                    "[ [" 'Info-prev
                    "] ]" 'Info-next
                    "SPC" nil)

(provide 'config-help)

;;; config-help.el ends here

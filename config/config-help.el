;;; config-help.el --- additional help functions

;;; Commentary:

;;; Code:

(require 'config-package)
(require 'config-evil)

(use-package help+
  :demand t)

(use-package help-mode+
  :demand t)

(use-package help-fns+
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

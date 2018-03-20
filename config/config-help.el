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

(require 'ace-link)
(general-define-key :states  'normal
                    :keymaps 'help-mode-map
                    "C-t"    'help-go-back
                    "C-]"    'help-follow
                    "f"      'ace-link-help)

(general-define-key :states  'normal
                    :keymaps 'Info-mode-map
                    "[ ["    'Info-prev
                    "] ]"    'Info-next
                    "f"      'ace-link-info
                    "SPC" nil)

(provide 'config-help)

;;; config-help.el ends here

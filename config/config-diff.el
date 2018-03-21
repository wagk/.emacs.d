;;; config-diff.el --- ediff things

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)

(use-package evil-ediff
  :after (evil)
  :demand t)

(provide 'config-diff)

;;; config-diff.el ends here

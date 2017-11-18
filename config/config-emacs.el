;;; config-emacs.el --- packages that make emacs better

;;; Commentary:

;;; Code:
(require 'config-package)

(use-package restart-emacs
  :commands restart-emacs)

;; call desktop-clear to reset the desktop file
(desktop-save-mode 1)

(provide 'config-emacs)

;;; config-emacs.el ends here

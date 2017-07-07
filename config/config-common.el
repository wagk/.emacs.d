;;; config-common.el --- Headers and packages other packages need

;;; Commentary:

;;; Code:
(require 'config-package)

(with-no-warnings
  (require 'cl))

(use-package dash
             :ensure t)

(use-package s
             :ensure t)

(provide 'config-common)

;;; config-common.el ends here

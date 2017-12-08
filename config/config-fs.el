;;; config-fs.el --- filesystem configs

;;; Commentary:

;;; Code:
(require 'config-package)


(define-key dired-mode-map (kbd "C-l") 'dired-up-directory)


(provide 'config-fs)

;;; config-fs.el ends here

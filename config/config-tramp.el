;;; config-tramp.el --- tramp/ssh configuration

;;; Commentary:

;;; Code:
(require 'config-package)

(require 'tramp)

;; configure tramp to use putty on windows
(when (or (eq system-type 'ms-dos)
          (eq system-type 'windows-nt))
  (setq-default tramp-auto-save-directory temporary-file-directory
                tramp-default-method "plink")
  )

(provide 'config-tramp)

;;; config-tramp.el ends here

;;; config-cygwin.el --- cygwin configuration for windows

;;; Commentary:

;;; Code:
(require 'config-package)

(when (or (eq system-type 'ms-dos)
          (eq system-type 'windows-nt))
  (setq explicit-shell-file-name "c:/cygwin64/bin/bash.exe"
        shell-file-name explicit-shell-file-name)
  (add-to-list 'exec-path "c:/cygwin64/bin")
  )

(provide 'config-cygwin)

;;; config-cygwin.el ends here

;;; config-shell.el --- shell configuration

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)

;; (when (or (eq system-type 'ms-dos)
;;           (eq system-type 'windows-nt))
;;   (setq explicit-shell-file-name "c:/cygwin64/bin/bash.exe"
;;         shell-file-name explicit-shell-file-name)
;;   (add-to-list 'exec-path "c:/cygwin64/bin")
;;   )

(use-package multi-term
  :after evil
  :init
  (evil-ex-define-cmd "te[rminal]" 'multi-term)
  :config
  ;; (cond ((or (eq system-type 'ms-dos)
  ;;            (eq system-type 'windows-nt)) (setq multi-term-program "cmd"))
  ;;       (t (setq multi-term-program "/bin/bash")))

  (add-hook 'term-mode-hook 'turn-off-evil-mode)
  )

(add-hook 'eshell-mode-hook 'turn-off-evil-mode)

(use-package powershell)
(provide 'config-shell)

;;; config-shell.el ends here

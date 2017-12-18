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
  :bind (:map term-mode-map
              ("RET" . term-send-input)
              )
  :init
  (evil-ex-define-cmd "te[rminal]" 'multi-term)
  :config
  (cond ((or (eq system-type 'ms-dos)
             (eq system-type 'windows-nt)) (setq multi-term-program "cmd"))
        (t (setq multi-term-program "/bin/bash")))
  )

(use-package powershell)

(evil-define-key 'normal term-mode
  "RET" 'term-send-input)

(evil-define-key 'insert term-mode
  "RET" 'term-send-input)

(provide 'config-shell)

;;; config-shell.el ends here

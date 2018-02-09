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

(add-hook 'comint-mode-hook 'turn-off-evil-mode)
;; (add-hook 'term-mode-hook 'turn-off-evil-mode)

(use-package multi-term
  :after evil
  :init
  (evil-ex-define-cmd "te[rminal]" 'multi-term)
  ;; I do not understand how this works, and it worries me some.
  (add-hook 'term-mode-hook #'(lambda ()
                                (evil-local-set-key 'motion (kbd "RET") 'term-send-input)
                                (evil-local-set-key 'insert (kbd "RET") 'term-send-input)
                                ))
  :config
  (evil-make-overriding-map term-mode-map)

  ;; I do not understand why this does *not* work and yet the lambda one does,
  ;; and it worries me quite a bit

  ;; (general-define-key
  ;;  :states '(motion insert)
  ;;  :keymaps 'local
  ;;  "RET" 'term-send-input)
  )

(use-package powershell)

(provide 'config-shell)

;;; config-shell.el ends here

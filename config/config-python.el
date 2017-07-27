;;; config-python.el --- python configuration file

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-prog)

;; ;; https://github.com/jorgenschaefer/elpy
;; (use-package elpy
;;   :ensure t
;;   :config
;;   (elpy-enable))

;; (add-hook 'python-mode-hook 'turn-on-ctags-auto-update-mode)
(add-hook 'python-mode-hook 'highlight-indent-guides-mode)

(provide 'config-python)

;;; config-python.el ends here

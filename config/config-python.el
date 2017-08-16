;;; config-python.el --- python configuration file

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-prog)
(require 'config-completion)

;; ;; https://github.com/jorgenschaefer/elpy
;; (use-package elpy
;;   :ensure t
;;   :config
;;   (elpy-enable))

;; (add-hook 'python-mode-hook 'turn-on-ctags-auto-update-mode)
(add-hook 'python-mode-hook 'highlight-indent-guides-mode)

(use-package company-jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook #'(lambda ()
                                  (add-to-list 'company-backends 'company-jedi))))


(provide 'config-python)

;;; config-python.el ends here

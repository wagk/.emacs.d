;;; config-lisp.el --- configuration for lisp dialect languages

;;; Commentary:

;;; Code:
(require 'config-package)

(use-package rainbow-delimiters
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  )

(use-package elisp-slime-nav
  :config
  (defun my-elisp-mode()
    (elisp-slime-nav-mode)
    (turn-on-eldoc-mode))
  (add-hook 'emacs-lisp-mode-hook 'my-elisp-mode)
  )

(provide 'config-lisp)

;;; config-lisp.el ends here

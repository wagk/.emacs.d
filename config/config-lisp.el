;;; config-lisp.el --- configuration for lisp dialect languages

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-lint)

(use-package rainbow-delimiters
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  )

(use-package suggest)

(use-package elmacro)

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :bind
  (:map elisp-slime-nav-mode-map
        ("C-:" . eval-last-sexp))
  :init
  (defun my-elisp-mode ()
    (elisp-slime-nav-mode)
    (turn-on-eldoc-mode))
  (add-hook 'emacs-lisp-mode-hook 'my-elisp-mode)
  )

(with-eval-after-load 'elisp-mode
  (general-define-key :states 'insert
                      :keymaps 'emacs-lisp-mode-map
                      "RET" 'comment-indent-new-line))

(provide 'config-lisp)

;;; config-lisp.el ends here

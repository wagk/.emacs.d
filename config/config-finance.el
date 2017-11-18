;;; config-finance.el --- finance tracking stuff, mostly ledger-mode

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)

(use-package ledger-mode
  :init
  (setq ledger-clear-whole-transactions 1)
  :config
  (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
  :mode ("\\.dat\\'"
         "\\.ledger\\'")
  )

(use-package flycheck-ledger)

(provide 'config-finance)

;;; config-finance.el ends here

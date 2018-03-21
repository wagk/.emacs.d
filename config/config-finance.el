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

;; Adds a transaction text object bound to `x' by default
;; https://github.com/atheriel/evil-ledger
(use-package evil-ledger
  :after (:all evil ledger-mode)
  :demand t
  :config
  (add-hook 'ledger-mode-hook #'evil-ledger-mode))

(use-package flycheck-ledger)

(provide 'config-finance)

;;; config-finance.el ends here

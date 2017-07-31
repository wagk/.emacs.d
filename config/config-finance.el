;;; config-finance.el --- finance tracking stuff, mostly ledger-mode

;;; Commentary:

;;; Code:
(require 'config-package)

(use-package ledger-mode
  :ensure t
  :config
  )

(use-package flycheck-ledger
  :ensure t
  :config
  )

(provide 'config-finance)

;;; config-finance.el ends here

;;; config-startup.el --- startup related things, like dashboards

;;; Commentary:

;;; Code:
(require 'config-package)

(use-package dashboard
  :disabled t
  :init
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-startup-banner nil))

(provide 'config-startup)

;;; config-startup.el ends here

;;; config-snippets.el --- Yasnippet configuration

;;; Commentary:
;; At the time of writing I'm getting a bit power-crazed with all the Emacs
;; configuration. This config file might get rolled into another config file
;; at some point in the future

;;; Code:
(require 'config-package)

(use-package yasnippet
  :config
  (yas-global-mode 1)
  )

(provide 'config-snippets)

;;; config-snippets.el ends here

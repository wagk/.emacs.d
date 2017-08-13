;;; config-draw.el --- Drawing packages

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-org)

;; https://github.com/skuro/plantuml-mode
(use-package plantuml-mode
  :ensure t
  :mode "\\.plantuml\\'"
  :config
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (when (boundp '/plantuml-jar)
    (setq plantuml-jar-path /plantuml-jar))
  )

(provide 'config-draw)

;;; config-draw.el ends here

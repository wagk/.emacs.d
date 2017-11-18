;;; config-draw.el --- Drawing packages

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-org)

;; https://github.com/skuro/plantuml-mode
(use-package plantuml-mode
  :mode "\\.plantuml\\'"
  :config
  (when (boundp '/plantuml-jar)
    (setq plantuml-jar-path /plantuml-jar))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages
   'org-babel-load-languages '((plantuml . t)))
  )

(provide 'config-draw)

;;; config-draw.el ends here

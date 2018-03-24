;;; config-tex.el --- text editing and formatting with latex

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)

(use-package auctex)

;; Adds a few textobjects into latex mode.
;; | Key | Description                          |
;; | $   | Inline math ($$)                     |
;; | \   | Display math (\[ \])                 |
;; | m   | TeX macros (\foo{})                  |
;; | E   | Tex environments (\begin{}...\end{}) |
(use-package evil-latex-textobjects
  :load-path "local-packages/"
  :demand t
  :general
  (:keymaps 'evil-latex-textobjects-inner-map
   "e" nil
   "E" 'evil-latex-textobjects-inner-env)
  (:keymaps 'evil-latex-textobjects-outer-map
   "e" nil
   "E" 'evil-latex-textobjects-an-env)
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-evil-latex-textobjects-mode))

(provide 'config-tex)

;;; config-tex.el ends here

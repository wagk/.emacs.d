;;; config-text.el --- Text related packages

(use-package aggressive-fill-paragraph
  :ensure (:host github :repo "davidshepherd7/aggressive-fill-paragraph-mode")
  :commands (aggressive-fill-paragraph-mode)
  :general
  (:states 'normal
   :prefix my-default-evil-leader-key
   "g w" 'aggressive-fill-paragraph-mode)
  :hook ((org-mode-hook . aggressive-fill-paragraph-mode)
         (markdown-mode-hook . aggressive-fill-paragraph-mode)))

(use-package aggressive-indent
  :ensure (:host github :repo "malabarba/aggressive-indent-mode")
  :commands (aggressive-indent-mode)
  :custom
  (aggressive-indent-comments-too t)
  :general
  (:states 'normal
   :prefix my-default-evil-leader-key
   "=" 'aggressive-indent-mode))

(provide 'config-text)

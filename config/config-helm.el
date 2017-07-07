;;; config-helm.el --- Helm configuration

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)

(use-package helm
  :ensure t
  :bind (:map helm-map
              ("C-w" . evil-delete-backward-word)
              ("S-SPC" . helm-select-action)
              ("TAB" . helm-execute-persistent-action))
  :config
  (setq helm-recentf-fuzzy-match t
        helm-locate-fuzzy-match nil ;; locate fuzzy is worthless
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-semantic-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-lisp-fuzzy-completion t
        helm-completion-in-region-fuzzy-match t
        helm-split-window-in-side-p t)
  (progn (helm-autoresize-mode)
         (setq helm-autoresize-min-height 40 ;; these values are %
               helm-autoresize-max-height 40))
  (evil-leader/set-key "<SPC>" 'helm-M-x))

(helm-mode 1)

(provide 'config-helm)

;;; config-helm.el ends here

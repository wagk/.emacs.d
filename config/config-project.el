;;; config-project.el --- projectile and project management packages

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-helm)
(require 'config-org)
(require 'config-evil)

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (add-hook 'after-init-hook #'projectile-mode))

(use-package helm-projectile
  :ensure t
  :after (helm
          evil
          evil-leader)
  :config
  (evil-leader/set-key
    "p" 'helm-projectile))

;; For when we're more comfortable with org
;; (use-package org-projectile
;;   :ensure t
;;   :after org
;;   :config
;;   (org-projectile:per-repo)
;;   (setq org-projectile:per-repo-filename ".todo.org"
;;         org-agenda-files (append org-agenda-files
;;                                  (org-projectile:todo-files))))

;; "]"        'org-projectile:template-or-project

(provide 'config-project)

;;; config-project.el ends here

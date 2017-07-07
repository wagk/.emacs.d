;;; config-git.el --- git-related configuration

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)

(use-package magit
  :ensure t
  :after evil-leader
  :config
  (evil-leader/set-key "," 'magit-status))

(use-package git-gutter+
  :ensure t
  :config
  (use-package git-gutter-fringe+
    :ensure t)
  (global-git-gutter+-mode 1))

(provide 'config-git)

;;; config-git.el ends here

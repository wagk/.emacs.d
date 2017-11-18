;;; config-git.el --- git-related configuration

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)
(require 'config-indent)

(use-package magit
  :commands magit-status
  :init
  (evil-leader/set-key
    ", ," 'magit-status)
  :config
  (eval-after-load 'aggressive-fill-paragraph
    '(add-hook 'git-commit-setup-hook 'aggressive-fill-paragraph-mode))
  (eval-after-load 'fill-column-indicator
    '(add-hook 'git-commit-setup-hook 'turn-on-fci-mode))
  )

(use-package evil-magit
  :after magit)

;; https://github.com/nonsequitur/git-gutter-plus
(use-package git-gutter+
  :bind (:map evil-normal-state-map
              ("[ h" . git-gutter+-previous-hunk)
              ("] h" . git-gutter+-next-hunk))
  :init
  (evil-leader/set-key
    "h s" 'git-gutter+-stage-hunks
    "h u" 'git-gutter+-revert-hunks
    "h p" 'git-gutter+-show-hunk)
  (global-git-gutter+-mode))

(use-package git-gutter-fringe+
  :after git-gutter+)

(provide 'config-git)

;;; config-git.el ends here

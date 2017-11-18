;;; config-git.el --- git-related configuration

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)
(require 'config-indent)

(use-package evil-magit)

(use-package magit
  :commands magit-status
  :init
  (require 'evil-leader)
  (evil-leader/set-key
    ", ," 'magit-status)
  :config
  (require 'evil-magit)
  (eval-after-load 'aggressive-fill-paragraph
    '(add-hook 'git-commit-setup-hook 'aggressive-fill-paragraph-mode))
  (eval-after-load 'fill-column-indicator
    '(add-hook 'git-commit-setup-hook 'turn-on-fci-mode))
  )

;; https://github.com/nonsequitur/git-gutter-plus
(use-package git-gutter+
  :bind (:map evil-normal-state-map
              ("[ h" . git-gutter+-previous-hunk)
              ("] h" . git-gutter+-next-hunk))
  :config
  (evil-leader/set-key
    "h s" 'git-gutter+-stage-hunks
    "h u" 'git-gutter+-revert-hunks
    "h p" 'git-gutter+-show-hunk)
  (use-package git-gutter-fringe+)
  (global-git-gutter+-mode 1))

(provide 'config-git)

;;; config-git.el ends here

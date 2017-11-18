;;; config-git.el --- git-related configuration

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)

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
              ("] h" . git-gutter+-next-hunk)
              ("g h s" . git-gutter+-stage-hunks)
              ("g h u" . git-gutter+-revert-hunks)
              ("g h p" . git-gutter+-show-hunk)
              )
  :init
  (global-git-gutter+-mode)
  )

(use-package git-gutter-fringe+
  :if (display-graphic-p)
  :after git-gutter+
  )

(provide 'config-git)

;;; config-git.el ends here

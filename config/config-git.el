;;; config-git.el --- git-related configuration

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)
(require 'config-indent)

(use-package magit
  :ensure t
  :after evil-leader
  :config
  (eval-after-load 'aggressive-fill-paragraph
    '(add-hook 'git-commit-setup-hook 'aggressive-fill-paragraph-mode))
  (eval-after-load 'fill-column-indicator
    '(add-hook 'git-commit-setup-hook 'turn-on-fci-mode))
  (evil-leader/set-key
    ", ," 'magit-status))

;; https://github.com/nonsequitur/git-gutter-plus
(use-package git-gutter+
  :ensure t
  :bind (:map evil-normal-state-map
              ("[ h" . git-gutter+-previous-hunk)
              ("] h" . git-gutter+-next-hunk))
  :config
  (evil-leader/set-key
    "h s" 'git-gutter+-stage-hunks
    "h u" 'git-gutter+-revert-hunks
    "h p" 'git-gutter+-show-hunk)
  (use-package git-gutter-fringe+
    ;; :if (not (display-graphic-p))
    :ensure t)
  (global-git-gutter+-mode 1))

(provide 'config-git)

;;; config-git.el ends here

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
  (progn (require 'aggressive-fill-paragraph)
         (add-hook 'git-commit-setup-hook 'aggressive-fill-paragraph-mode))
  (evil-leader/set-key
    ", ," 'magit-status))

;; https://github.com/nonsequitur/git-gutter-plus
(use-package git-gutter+
  :ensure t
  :after evil evil-leader
  :bind (:map evil-normal-state-map
              ("[ c" . git-gutter+-previous-hunk)
              ("] c" . git-gutter+-next-hunk))
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

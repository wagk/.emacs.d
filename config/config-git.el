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

;; https://github.com/nonsequitur/git-gutter-plus
(use-package git-gutter+
  :ensure t
  :bind (:map evil-normal-state-map
              ("[ c" . git-gutter+-previous-hunk)
              ("] c" . git-gutter+-next-hunk))
  :config
  ;; (evil-leader/set-key
  ;;   "hs" 'git-gutter+-stage-hunks
  ;;   "hu" 'git-gutter+-revert-hunks
  ;;   "hp" 'git-gutter+-show-hunk)
  (use-package git-gutter-fringe+
    :ensure t)
  (global-git-gutter+-mode 1))

(provide 'config-git)

;;; config-git.el ends here

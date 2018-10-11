(use-package auto-package-update
  :commands (auto-package-update-now
             auto-package-update-at-time
             auto-package-update-maybe)
  :custom
  (auto-package-update-delete-old-versions t
                                           "We already version them on
                                           git")
  (auto-package-update-prompt-before-update t
                                            "NO SURPRISES")
  (auto-package-update-interval 14
                                "update once every 2 weeks (the count
                                is in days)"))

(use-package evil-collection
      :after (evil)
      :straight (:host github
			 :repo "emacs-evil/evil-collection"
			 :branch "master")
      :custom
      (evil-collection-setup-minibuffer t)
      :config
      ;;NOTE: note that this REQUIRES the var `evil-want-integration' to be NIL
      (evil-collection-init))

(use-package restart-emacs
  :straight (:host github :repo "iqbalansari/restart-emacs" :branch "master")
  :commands (restart-emacs))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(window-divider-mode -1)

(setq initial-scratch-message
      "It is possible to commit no mistakes and still lose.
That is not weakness. That is life.

")

(setq initial-major-mode 'fundamental-mode)

(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-screen t)

(setq-default require-final-newline t)

(setq ring-bell-function 'ignore)

(setq w32-pipe-read-delay 0)

(setq-default tab-width 4)

(setq tab-always-indent 'complete)

(setq-default indent-tabs-mode nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'after-make-frame-functions 'select-frame)

(customize-set-variable 'sentence-end-double-space nil)

(add-hook 'after-change-major-mode-hook '(lambda () (modify-syntax-entry ?_ "w")))

(setq delete-old-versions t
      backup-by-copying t
      version-control t
      kept-new-versions 20
      kept-old-versions 5
      vc-make-backup-files t)
(setq savehist-save-minibuffer-history 1
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq history-length t
      history-delete-duplicates t)
(savehist-mode 1)

(setq x-stretch-cursor t)

(when (>= emacs-major-version 26)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(customize-set-variable 'frame-background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)

(use-package solarized-theme
  :demand t
  :custom
  (solarized-use-variable-pitch nil)
  (solarized-distinct-fringe-background nil)
  (solarized-high-contrast-mode-line nil)
  (solarized-use-less-bold t)
  (solarized-use-more-italic nil)
  (solarized-scale-org-headlines nil)
  (solarized-height-minus-1 1.0)
  (solarized-height-plus-1 1.0)
  (solarized-height-plus-2 1.0)
  (solarized-height-plus-3 1.0)
  (solarized-height-plus-4 1.0)
  :config
  (load-theme 'solarized-dark t))

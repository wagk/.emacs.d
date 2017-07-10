;;; config-helm.el --- Helm configuration

;;; Commentary:
;; Really this is for fuzzy finding in general

;;; Code:
(require 'config-package)
(require 'config-evil)
(require 'config-git)

;; BEGIN CONTINGENCIES

;; Install ivy as a contingency
(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t))

(use-package swiper
  :ensure t
  :after ivy)

(use-package helm
  :ensure t
  :demand t
  :after evil
  :bind(:map helm-map
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
  ;; evil-related configuration
  (evil-leader/set-key
    "<SPC>" 'helm-M-x
    "S-<SPC>" 'helm-resume
    "y" 'helm-show-kill-ring
    "h" 'helm-apropos
    "-" 'helm-find-files
    "_" 'helm-mini
    "b" 'helm-bookmarks))

;; TODO: when defining helm desckeys make sure a global binding is also presentw
;; C-h seems broken (We've been overwriting it to enable terminal backspace)

(use-package helm-describe-modes
  :ensure t
  :after helm
  :bind (("C-h m" . helm-describe-modes))
  ;; :config
  ;; (evil-leader/set-key "m" 'helm-describe-modes)
  )

(use-package helm-descbinds
  :ensure t
  :after helm
  :config
  (helm-descbinds-mode))

(use-package helm-swoop
  :ensure t
  :after helm evil
  :bind (:map helm-swoop-map
              ("C-w" . evil-delete-backward-word))
  ;; no annoying under mouse highlights
  ;;(setq helm-swoop-pre-input-function (lambda () nil))
  )

(use-package helm-fuzzier
  :ensure t
  :after helm
  :config
  (helm-fuzzier-mode 1))

(use-package helm-flx
  :ensure t
  :after helm
  :config (helm-flx-mode 1))

(use-package helm-hunks
  :ensure t
  :after helm git-gutter+
  :commands (helm-hunks
             helm-hunks-current-buffer
             helm-hunks-staged
             helm-hunks-staged-current-buffer)
  :config
  (add-hook 'helm-hunks-refresh-hook 'git-gutter+-refresh)
  (setq helm-hunks-preview-diffs t)
  )

(helm-mode 1)

(provide 'config-helm)

;;; config-helm.el ends here

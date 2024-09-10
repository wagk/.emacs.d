(setq frame-background-mode 'light)

(remove-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; (global-display-fill-column-indicator-mode -1)

(use-package solarized-definitions
  :ensure (:host github :repo "sellout/emacs-color-theme-solarized" :main nil)
  :config
  (load-theme 'solarized t))

;; (load-theme 'modus-vivendi t)
;; (load-theme 'solarized t)

(with-eval-after-load 'faces
  (set-face-attribute 'mode-line-inactive nil
                      :box '(:line-width 3)
                      :foreground 'unspecified
                      :background 'unspecified
                      :inverse-video nil
                      :inherit 'default)
  (set-face-attribute 'mode-line-active nil
                      :box '(:line-width 3)
                      :foreground 'unspecified
                      :background 'unspecified
                      :inverse-video nil
                      :inherit '(default highlight)))

(with-eval-after-load 'dired-filter
  (set-face-attribute 'dired-filter-group-header nil
                      :background 'unspecified
                      :foreground 'unspecified
                      :inherit 'default))

(provide 'config-theme-term)

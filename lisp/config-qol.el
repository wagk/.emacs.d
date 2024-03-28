;;; config-qol.el --- Packages that add quality of life to existing emacs packages

(require 'config-emacs)

(use-package fancy-compilation
  :after compile
  :custom
  (fancy-compilation-override-colors nil)
  (fancy-compilation-scroll-output 'first-error)
  (fancy-compilation-quiet-prolog nil)
  (fancy-compilation-quiet-prelude nil)
  :config
  (fancy-compilation-mode))

(use-package which-key
  :ensure (:host github :repo "justbur/emacs-which-key")
  :demand t
  :init
  (with-eval-after-load 'evil
    (customize-set-value 'which-key-allow-evil-operators t)
    ;; note that this is marked as *experimental*
    (customize-set-value 'which-key-show-operator-state-maps t))
  :custom
  (which-key-use-C-h-commands nil)
  (which-key-is-verbose t)
  (which-key-popup-type 'minibuffer)
  (which-key-side-window-max-width 0.33)
  (which-key-max-display-columns nil)
  (which-key-show-docstrings t)
  (which-key-side-window-location 'bottom)
  ;; :general
  ;; (:states 'normal
  ;;  "C-h M-k" 'which-key-show-keymap
  ;;  "C-h M-m" 'which-key-show-full-major-mode)
  :config
  (which-key-mode))

(provide 'config-qol)

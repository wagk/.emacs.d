;;; config-webdev.el --- Typescript, javascript, and related topics

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-helm)
(require 'config-evil)

;; TODO(pangt): when this takes a relative path, give a relative path
;; (currently it's only relative to user-init-dir)
(load-user-config-file "./config/config-typescript.el")

(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  )

(use-package js2-mode
  :pin gnu
  :mode ("\\.js\\'" . js2-mode)
  )

(use-package emmet-mode
  :bind (:map emmet-mode-keymap
              ("C-j" . emmet-next-edit-point)
              ("C-k" . emmet-prev-edit-point))
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable emmet's css abbreviation.
  :config
  (setq emmet-move-cursor-between-quotes t) ;; default nil
  )

;; not sure if this inherits from prog-mode
(use-package groovy-mode
  :mode ("\\Jenkinsfile\\'" . groovy-mode)
  :config
  (progn (require 'fill-column-indicator)
         (add-hook 'groovy-mode-hook 'turn-on-fci-mode))
  (progn (require 'evil-core)
         (evil-define-key 'insert 'groovy-mode-map
           (kbd "RET") 'comment-indent-new-line))
  (progn (require 'hl-todo)
         (add-hook 'groovy-mode-hook 'hl-todo-mode))
  (add-hook 'groovy-mode-hook #'/treat-underscore-as-word)
  )

(use-package php-mode
  :mode ("\\.php\\'" . php-mode))

(use-package dockerfile-mode
  :mode ("\\Dockerfile\\'" . dockerfile-mode)
  :config
  (add-hook 'dockerfile-mode-hook 'hl-todo-mode))

(use-package json-mode)

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'orgtbl-mode)
  (add-hook 'markdown-mode-hook 'turn-on-fci-mode))

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook 'turn-on-fci-mode))

(use-package helm-emmet)

(provide 'config-webdev)

;;; config-webdev.el ends here

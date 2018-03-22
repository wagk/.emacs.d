;;; config-webdev.el --- Typescript, javascript, and related topics

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-helm)
(require 'config-evil)

;; TODO(pangt): when this takes a relative path, give a relative path
;; (currently it's only relative to user-init-dir)
(load-user-config-file "config-typescript.el")

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

;; not sure if this inherits from prog-mode
(use-package groovy-mode
  :mode ("\\Jenkinsfile\\'" . groovy-mode)
  :general
  (:states 'insert
   :keymaps 'groovy-mode-map
   "RET" 'comment-indent-new-line)
  :config
  (progn (require 'fill-column-indicator)
         (add-hook 'groovy-mode-hook 'turn-on-fci-mode))
  (progn (require 'hl-todo)
         (add-hook 'groovy-mode-hook 'hl-todo-mode))
  (add-hook 'groovy-mode-hook #'/treat-underscore-as-word)
  )

(use-package php-mode
  :mode ("\\.php\\'" . php-mode)
  :general
  (:states 'insert
   :keymaps 'php-mode-map
   "RET" 'comment-indent-new-line))

(use-package dockerfile-mode
  :mode ("\\Dockerfile\\'" . dockerfile-mode)
  :config
  (add-hook 'dockerfile-mode-hook 'hl-todo-mode))

(use-package json-mode)

(use-package markdown-mode
  :config
  (require 'org)
  (add-hook 'markdown-mode-hook 'orgtbl-mode)
  (add-hook 'markdown-mode-hook 'turn-on-fci-mode))

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook 'turn-on-fci-mode))

(use-package helm-emmet)

(provide 'config-webdev)

;;; config-webdev.el ends here

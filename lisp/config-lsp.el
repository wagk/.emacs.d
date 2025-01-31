(use-package spinner
  :ensure (:host github :repo "emacs-straight/spinner" :branch "master"))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-enable-links nil "We don't rely on clickable links and they clutter the color")
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-file-watch-threshold 1500)
  (lsp-imenu-index-function #'lsp-imenu-create-categorized-index)
  (lsp-enable-file-watchers nil)
  :init
  ;; from https://github.com/minad/corfu/wiki#user-content-example-configuration-with-flex
  (with-eval-after-load 'orderless
    (setq lsp-completion-provider :none)
    (cl-defun --lsp-orderless-completion ()
      (setf (->> completion-category-defaults
                 (alist-get 'lsp-capf)
                 (alist-get 'styles))
            '(orderless flex basic)))
    (add-hook 'lsp-completion-mode-hook #'--lsp-orderless-completion))

  ;; (with-eval-after-load 'prescient
  ;;   (setq lsp-completion-provider :none)
  ;;   (cl-defun --lsp-prescient-completion ()
  ;;     (setf (->> completion-category-defaults
  ;;                (alist-get 'lsp-capf)
  ;;                (alist-get 'styles))
  ;;           '(prescient flex basic)))
  ;;   (add-hook 'lsp-completion-mode-hook #'--lsp-prescient-completion))
  :config
  ;; https://github.com/emacs-lsp/lsp-mode/issues/3577
  ;; (delete 'lsp-terraform lsp-client-packages)
  ;; this is necessary since `lsp-command-map' is not autoloadable
  (general-define-key
   :states 'normal
   :prefix my-default-evil-leader-key
   "l" lsp-command-map)
  ;; (lsp-mode-map
  ;;  :states 'normal
  ;;  :prefix my-default-evil-leader-key
  ;;   "l a a" 'lsp-execute-code-action))
  ;; practically disabling it
  (custom-set-faces `(lsp-flycheck-warning-unnecessary-face
                      ((t (:foreground unspecified
                           :underline unspecified)))))
  (with-eval-after-load 'lsp-modeline
    (setq lsp-modeline-code-action-fallback-icon "Actions Available"))

  (with-eval-after-load 'lsp-rust
    (customize-set-value 'lsp-rust-clippy-preference "on")
    (customize-set-value 'lsp-rust-cfg-test t)))

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-peek-enable t)
  :hook (lsp-mode-hook . lsp-ui-mode))

(use-package lsp-ui-imenu
  :after lsp-mode
  :ensure nil
  :general
  (:keymaps 'lsp-command-map
   "i i" #'lsp-ui-imenu))

(use-package lsp-ui-peek
  :after lsp-ui
  :ensure nil
  :general
  (lsp-ui-peek-mode-map
   "C-j" 'lsp-ui-peek--select-next
   "C-k" 'lsp-ui-peek--select-prev
   "M-j" 'lsp-ui-peek--select-next
   "M-k" 'lsp-ui-peek--select-prev))

(use-package lsp-ui-doc
  :after lsp-ui
  :ensure nil
  :custom
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-position 'at-point)
  :hook
  (lsp-ui-doc-mode-hook . (lambda ()
                            (general-define-key
                             :keymaps 'lsp-command-map
                              "h h" (if lsp-ui-doc-mode
                                        'lsp-ui-doc-glance
                                      'lsp-describe-thing-at-point)))))

(use-package consult-lsp
  :after (consult lsp-mode)
  :general
  (:keymaps 'lsp-command-map
   "f f" #'consult-lsp-file-symbols
   "f a" #'consult-lsp-symbols
   "f d" #'consult-lsp-diagnostics))

(use-package lsp-focus
  :after (lsp focus)
  :hook
  (focus-mode-hook . #'lsp-focus-mode))

(provide 'config-lsp)

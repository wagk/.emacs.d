(require 'use-package)

(setq-default tab-always-indent 'complete)

;; TODO: Figure out how to delete an entry from the completion
;; history
(use-package vertico
  :demand t
  :straight t
  :custom
  (vertico-count 23)
  :general
  (vertico-map
   "M-j" 'vertico-next
   "M-k" 'vertico-previous)
  ;; this sounds weird but "scroll down" here means "go back", which
  ;; visually looks like scrolling _up_. Swap up and down to be more
  ;; intuitive
  (vertico-map
   :states '(insert normal)
   "C-u" 'vertico-scroll-down
   "C-d" 'vertico-scroll-up)
  :config
  (vertico-mode))

(use-package prescient
  :straight (:host github
             :repo "radian-software/prescient.el"
             :files (:defaults "/*.el"))
  :config
  (with-eval-after-load 'vertico
    (vertico-prescient-mode))
  (with-eval-after-load 'corfu
    (corfu-prescient-mode)))

(use-package consult
  :demand t
  :straight t
  :general
  (:states 'normal
   "g /" #'consult-line)
  :config
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "bb" 'consult-buffer)
    (evil-ex-define-cmd "ii" 'consult-imenu))
  (advice-add 'repeat-complex-command :override #'consult-complex-command)
  (setq completion-in-region-function #'consult-completion-in-region))

(use-package consult-xref
  :ensure nil
  :straight nil
  :after (:all consult xref)
  :config
  (setq xref-show-xrefs-function 'consult-xref))

(use-package consult-git-log-grep
  :straight (:host github :repo "ghosty141/consult-git-log-grep")
  :commands (consult-git-log-grep)
  :config
  (with-eval-after-load 'magit
    (set-default 'consult-git-log-grep-open-function #'magit-show-commit)))

(use-package marginalia
  :straight t
  :after consult
  :config (marginalia-mode))

(use-package corfu
  :straight t
  ;; normal tab-completion seems to beat this with how it does
  ;; incremental completions
  ;; :demand t ;; not sure how the :general config will affect this
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.01)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-quit-at-boundary separator)
  (corfu-quit-no-match t)
  :commands
  (corfu-mode
   global-corfu-mode)
  :general
  (corfu-map
   "<return>" #'corfu-complete)
  :custom-face
  (corfu-bar ((t (:inherit region))))
  (corfu-current ((t (:inherit sol-subtle-background
                      :bold t
                      :foreground unspecified
                      :background unspecified))))
  (corfu-default ((t (:inherit sol-subtle-background
                      :background unspecified))))
  (corfu-border ((t (:inherit corfu-default
                     :background unspecified))))
  (corfu-deprecated ((t (:inherit shadow :strike-through nil)))))

(use-package embark
  :straight t
  :after vertico
  :commands (embark-act
             embark-dwim
             embark-bindings
             embark-prefix-help-command)
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :general
  (vertico-map
   "C-<SPC>" 'embark-act)
  (:states 'motion
   "C-<SPC>" 'embark-act))

(use-package embark-consult
  :straight t)

(use-package cape
  :straight t
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line))

(provide 'config-completions)

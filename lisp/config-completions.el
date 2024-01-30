(require 'use-package)

(setq-default tab-always-indent 'complete)

;; TODO: Figure out how to delete an entry from the completion
;; history
(use-package vertico
  :demand t
  :elpaca t
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
  :elpaca (:host github
             :repo "radian-software/prescient.el"
             :files (:defaults "/*.el"))
  :config
  (with-eval-after-load 'vertico
    (vertico-prescient-mode))
  (with-eval-after-load 'corfu
    (corfu-prescient-mode)))

(use-package consult
  :demand t
  :elpaca t
  :general
  (:states 'normal
   "g /" #'consult-line)
  :config
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "bb" 'consult-buffer)
    (evil-ex-define-cmd "ii" 'consult-imenu)
    (evil-ex-define-cmd "ia" 'consult-imenu-multi))
  (advice-add 'repeat-complex-command :override #'consult-complex-command)
  (setq completion-in-region-function #'consult-completion-in-region))

(use-package consult-xref
  :ensure nil
  :elpaca nil
  :after (:all consult xref)
  :config
  (setq xref-show-xrefs-function 'consult-xref))

(use-package consult-git-log-grep
  :elpaca (:host github :repo "ghosty141/consult-git-log-grep")
  :commands (consult-git-log-grep)
  :config
  (with-eval-after-load 'magit
    (set-default 'consult-git-log-grep-open-function #'magit-show-commit)))

(use-package consult-dir
  :elpaca t
  :commands consult-dir
  :custom
  (consult-dir-default-command #'find-file)
  :init
  (with-eval-after-load 'evil
    (--evil-define-splits "mm" #'consult-dir)))

(use-package marginalia
  :elpaca t
  :after consult
  :config (marginalia-mode))

(use-package corfu
  :elpaca t
  ;; normal tab-completion seems to beat this with how it does
  ;; incremental completions
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
  (corfu-border ((t (:inherit corfu-default
                     :background unspecified))))
  (corfu-deprecated ((t (:inherit shadow :strike-through nil))))
  :config
  (global-corfu-mode))

(use-package embark
  :elpaca t
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
  :elpaca t)

(use-package cape
  :elpaca t
  :demand t
  :config
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line))

(use-package yasnippet-capf
  :elpaca t
  :after (:all cape yasnippet)
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(provide 'config-completions)

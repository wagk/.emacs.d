(require 'use-package)

(setq tab-always-indent 'complete)

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
    (evil-ex-define-cmd "bb" 'consult-buffer))
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
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-quit-at-boundary nil)
  :config
  (global-corfu-mode)
  :general
  (corfu-map
   "<return>" #'corfu-complete)
  :custom-face
  (corfu-current ((t (:inherit completions-highlight :bold t))))
  (corfu-default ((t (:inherit secondary-selection))))
  (corfu-border ((t (:inherit default))))
  (corfu-bar ((t (:inherit region)))))

;; TODO (pangt): consider using cape https://github.com/minad/cape
(provide 'config::completions)

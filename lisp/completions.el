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
                    :files (:defaults "vertico-prescient.el"))
  :after vertico
  :config
  (vertico-prescient-mode))

(use-package consult
  :straight t
  :config
  (evil-ex-define-cmd "bb" 'consult-buffer)
  (advice-add 'repeat-complex-command :override #'consult-complex-command)
  (setq completion-in-region-function #'consult-completion-in-region))

(use-package marginalia
  :straight t
  :after consult
  :config (marginalia-mode))


(provide 'config::completions)

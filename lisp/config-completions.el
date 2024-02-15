(require 'use-package)

(setq-default tab-always-indent 'complete)

;; TODO: Figure out how to delete an entry from the completion
;; history
(use-package vertico
  :demand t
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

(use-package orderless
  :config
  (add-to-list 'completion-styles 'orderless))

(use-package prescient
  :ensure (:host github
           :repo "radian-software/prescient.el"
           :files (:defaults "/*.el"))
  :config
  (with-eval-after-load 'vertico
    (vertico-prescient-mode))
  (with-eval-after-load 'corfu
    (corfu-prescient-mode)))

(use-package consult
  :demand t
  :general
  (:states 'normal
   "g /" #'consult-line)
  :config
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "bb" 'consult-buffer)
    (evil-ex-define-cmd "ii" 'consult-imenu)
    (evil-ex-define-cmd "ia" 'consult-imenu-multi))
  (advice-add 'repeat-complex-command :override #'consult-complex-command)
  (setq completion-in-region-function #'(lambda (&rest args)
                                          (apply (if vertico-mode
                                                     #'consult-completion-in-region
                                                   #'completion--in-region)
                                                 args))))

(use-package consult-xref
  :ensure nil
  :after (:all consult xref)
  :config
  (setq xref-show-xrefs-function 'consult-xref))

(use-package consult-git-log-grep
  :ensure (:host github :repo "ghosty141/consult-git-log-grep")
  :commands (consult-git-log-grep)
  :after (magit consult)
  :init
  (evil-ex-define-cmd "gll" #'consult-git-log-grep)
  :config
  (set-default 'consult-git-log-grep-open-function #'magit-show-commit))

(use-package consult-dir
  :commands consult-dir
  :init
  (with-eval-after-load 'evil
    (--evil-define-splits "mm" #'consult-dir))

  (with-eval-after-load 'project
    (cl-defun --maybe-project-find-file ()
      (interactive)
      (if-let ((project (project-current)))
          (project-find-file)
        (find-file)))
    (setq consult-dir-default-command #'--maybe-project-find-file)))

(use-package marginalia
  :after consult
  :demand t
  :config (marginalia-mode))

(use-package corfu
  ;; normal tab-completion seems to beat this with how it does
  ;; incremental completions
  :demand t
  :custom
  (corfu-auto nil)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 3)
  (corfu-cycle t)
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary nil)
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
  (global-corfu-mode)
  (corfu-echo-mode))

(use-package embark
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

(use-package embark-consult)
  

(use-package cape
  :demand t
  :config
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(provide 'config-completions)

(require 'use-package)

(setq-default tab-always-indent 'complete)
(customize-set-variable 'completion-ignore-case t)
(customize-set-variable 'read-file-name-completion-ignore-case t)
(customize-set-variable 'read-buffer-completion-ignore-case t)
(customize-set-variable 'completions-detailed t)

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
  :custom
  (orderless-matching-styles '(orderless-literal
                               orderless-regexp
                               orderless-initialism
                               orderless-prefixes
                               orderless-flex))
  :config
  (with-eval-after-load 'consult
    ;; Adapted from
    ;; https://github.com/minad/consult/wiki#user-content-minads-orderless-configuration
    (cl-defun --orderless-consult-suffix ()
      "Regexp which matches the end of string with Consult tofu support."
      (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
          (format "[%c-%c]*$"
                  consult--tofu-char
                  (+ consult--tofu-char consult--tofu-range -1))
        "$"))
    (cl-defun --orderless-consult-dispatch (word _index _total)
      (cond
       ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
       ((string-suffix-p "$" word)
        `(orderless-regexp . ,(concat (substring word 0 -1)
                                      (--orderless-consult-suffix))))
       ;; File extensions
       ((and (or minibuffer-completing-file-name
                 (derived-mode-p 'eshell-mode))
             (string-match-p "\\`\\.." word))
        `(orderless-regexp . ,(concat "\\."
                                      (substring word 1)
                                      (--orderless-consult-suffix))))))
    (add-to-list 'orderless-style-dispatchers #'--orderless-consult-dispatch))

  ;; (add-to-list 'completion-styles 'orderless)
  (setq completion-styles '(orderless basic))
  (add-to-list 'completion-category-overrides
               '((file
                  (styles basic partial-completion)))))

(use-package prescient
  :disabled t
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
    (evil-set-command-property #'consult-buffer      :jump t)
    (evil-set-command-property #'consult-bookmark    :jump t)
    (evil-set-command-property #'consult-goto-line   :jump t)
    (evil-set-command-property #'consult-recent-file :jump t)
    (evil-set-command-property #'consult-imenu       :jump t)
    (evil-set-command-property #'consult-imenu-multi :jump t)
    (evil-set-command-property #'consult-line        :jump t)
    (evil-set-command-property #'consult-line-multi  :jump t)
    (evil-set-command-property #'consult-info        :jump t))
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "mb"  'consult-bookmark)
    (evil-ex-define-cmd "fm"  'consult-bookmark)
    (evil-ex-define-cmd "fn"  'consult-goto-line)
    (evil-ex-define-cmd "bb"  'consult-buffer)
    (evil-ex-define-cmd "fr"  'consult-recent-file)
    (evil-ex-define-cmd "fb"  'consult-buffer)
    ;; :fo is currently used by focus-mode, and I sort of like it that way right
    ;; now.
    (evil-ex-define-cmd "fout"  'consult-outline)
    (evil-ex-define-cmd "ii"  'consult-imenu)
    (evil-ex-define-cmd "ia"  'consult-imenu-multi)
    (evil-ex-define-cmd "fp"  'consult-yank-from-kill-ring) ;; p for paste
    (evil-ex-define-cmd "ff"  'consult-line)
    (evil-ex-define-cmd "fa"  'consult-line-multi)
    (evil-ex-define-cmd "fi"  'consult-info)
    (with-eval-after-load 'link-hint
      (evil-ex-define-cmd "fx"  'link-hint-open-link)
      (evil-ex-define-cmd "fyx"  'link-hint-copy-link)))
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

(use-package consult-dir
  :commands consult-dir
  :custom
  (consult-dir-default-command #'consult-dir-dired)
  :init
  (with-eval-after-load 'evil
    (--evil-define-splits "mm" #'consult-dir)))

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

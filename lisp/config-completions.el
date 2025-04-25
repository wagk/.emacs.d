(require 'use-package)

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
  :after config-theme
  :custom
  (orderless-matching-styles '(orderless-literal
                               orderless-regexp
                               orderless-initialism
                               orderless-prefixes
                               orderless-flex))
  :custom-face
  (orderless-match-face-0 ((default . (:foreground ,sol-green))))
  (orderless-match-face-1 ((default . (:foreground ,sol-blue))))
  (orderless-match-face-2 ((default . (:foreground ,sol-cyan))))
  (orderless-match-face-3 ((default . (:foreground ,sol-violet))))
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
  (setq completion-category-defaults nil)
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
  :after (config-theme evil project config-evil-helpers)
  :general
  (:states 'normal
   "g /" #'consult-line)
  :custom
  (consult-narrow-key ">")
  (consult-widen-key "<")
  :custom-face
  (consult-separator
   ((default . (:inherit sol-superlight-foreground))))
  (consult-line-number-wrapped
   ((default . (:inherit line-number))))
  :config
  (add-to-list 'consult-async-split-styles-alist
               '(slashperl :initial "/" :function consult--split-perl))
  (setq consult-async-split-style 'slashperl)

  (evil-set-command-property #'consult-buffer      :jump t)
  (evil-set-command-property #'consult-bookmark    :jump t)
  (evil-set-command-property #'consult-goto-line   :jump t)
  (evil-set-command-property #'consult-recent-file :jump t)
  (evil-set-command-property #'consult-imenu       :jump t)
  (evil-set-command-property #'consult-imenu-multi :jump t)
  (evil-set-command-property #'consult-line        :jump t)
  (evil-set-command-property #'consult-line-multi  :jump t)
  (evil-set-command-property #'consult-info        :jump t)
  (evil-set-command-property #'consult-ripgrep     :jump t)

  (--evil-define-splits "mb" #'consult-bookmark)
  (--evil-define-splits "fb" #'consult-buffer)
  (--evil-define-splits "bb" "fb")
  (--evil-define-splits "fd" #'(lambda () (interactive)
                                 (if (project-current) (project-find-file)
                                   (command-execute #'find-file))))
  (--evil-define-splits "fdd" #'(lambda () (interactive)
                                  (if (project-current) (project-find-dir)
                                    (command-execute #'dired))))
  (with-eval-after-load 'consult-todo
    ;; use `:pt' for project level todos
    (--evil-define-splits "ft" #'consult-todo))
  (evil-ex-define-cmd "mr"  'consult-recent-file)
  (evil-ex-define-cmd "fn"  'consult-goto-line)
    ;; :fo is currently used by focus-mode, and I sort of like it that way right
    ;; now.
    ;; (evil-ex-define-cmd "fout"  'consult-outline)
  (evil-ex-define-cmd "fi"  'consult-imenu)
  (evil-ex-define-cmd "ii"  'consult-imenu-multi)
  (evil-ex-define-cmd "fp"  'consult-yank-from-kill-ring) ;; p for paste
  (evil-ex-define-cmd "ff"  'consult-line)
  (evil-ex-define-cmd "fr"  'consult-register)
  (evil-ex-define-cmd "fl" '(lambda () (interactive)
                              (consult-line (thing-at-point 'symbol))))
  (evil-ex-define-cmd "fa"  'consult-line-multi)
  (evil-ex-define-cmd "fc"  'consult-compile-error)
  (evil-ex-define-cmd "foc" 'consult-focus-lines)

  (advice-add 'repeat-complex-command :override #'consult-complex-command)
  (setq completion-in-region-function #'(lambda (&rest args)
                                          (apply (if vertico-mode
                                                     #'consult-completion-in-region
                                                   #'completion--in-region)
                                                 args))))
;;   (when (executable-find "rg")
;;     (cl-defun --consult-ripgrep-single-file ()
;;       "Call `consult-ripgrep' for the current buffer (a single file).

;; Lifted from https://github.com/minad/consult/issues/407#issuecomment-1868143867"
;;       (interactive)
;;       (let ((consult-project-function (lambda (x) nil)))
;;         (consult-ripgrep (list (shell-quote-argument buffer-file-name)))))
;;     (evil-set-command-property #'--consult-ripgrep-single-file :jump t)
;;     (evil-ex-define-cmd "ff"  '--consult-ripgrep-single-file)
;;     (evil-ex-define-cmd "fa"  'consult-ripgrep)))

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
  (with-eval-after-load 'config-evil
    (--evil-define-splits "mm" #'consult-dir)))

(use-package marginalia
  :after consult
  :demand t
  :custom-face
  (marginalia-documentation ((((type nil)) . (:underline nil))))
  :config (marginalia-mode))

(use-package corfu
  ;; normal tab-completion seems to beat this with how it does
  ;; incremental completions
  :if (display-graphic-p)
  :demand t
  :after config-theme
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
  (corfu-current ((default . (:inverse-video t
                              :inherit sol-light-foreground))))
  (corfu-bar ((t . (:inherit region))))
  (corfu-border ((t . (:inherit corfu-default
                       :background unspecified))))
  (corfu-deprecated ((t . (:inherit shadow :strike-through nil)))))

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
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-line)
  (add-to-list 'completion-at-point-functions #'cape-dict))

(provide 'config-completions)

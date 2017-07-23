;;; config-evil.el --- evil-mode configuration

;;; Commentary:

;;; Code:
(require 'config-package)

;; TODO: figure out this
(defun /evil-paste-after-from-0 ()
  "I legitimately forgot what this does.
Probably copied it from stackoverflow"
  (interactive)
  (let ((evil-this-register ?0))
    (call-interactively 'evil-paste-after)))

(defun /evil-gt ()
  "Emulating vim's `gt' using frames."
  (interactive)
  (other-frame 1))

(defun /evil-gT ()
  "Emulating vim's `gT' using frames."
  (interactive)
  (other-frame -1))

(defun /lang-toggle ()
  "Input language toggle wrapper."
  (interactive)
  (toggle-input-method)
  (evil-append 1))

;; Overload shifts so that they don't lose the selection
(defun /evil-shift-left-visual ()
  "Keep visual selection after shifting left."
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun /evil-shift-right-visual ()
  "Same as -evil-shift-left-visual, but for the right instead."
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

;; Make my own leader keys and bake it into evil
(defvar /mapleader "<SPC>")

(defun /leader (keystroke)
  "Append our leader key onto KEYSTROKE."
  (concat /mapleader " " keystroke))

;; evil config
(use-package evil
  :ensure t
  :demand t
  :bind (("C-f" . universal-argument)
         ("C-u" . kill-whole-line)
         :map universal-argument-map
         ("C-u" . nil)
         ("C-f" . universal-argument-more)
         :map evil-insert-state-map
         ("C-j" . newline-and-indent)
         ("C-l" . evil-complete-next-line)
         ("C-L" . evil-complete-previous-line)
         ("C-k" . nil)
         ;; ("RET" . newline-and-indent) ;; this detonates term-mode
         :map evil-motion-state-map
         ("C-u" . evil-scroll-up)
         :map evil-normal-state-map
         ("gt"   . /evil-gt)
         ("gT"   . /evil-gT)
         ("C-\\" . /lang-toggle) ;; binding for eng <-> jap
         :map evil-visual-state-map
         ;; ("p"  . /evil-paste-after-from-0)
         (">>" . /evil-shift-right-visual)
         ("<<" . /evil-shift-left-visual)
         :map evil-inner-text-objects-map
         ("/" . /inner-forward-slash)
         :map evil-outer-text-objects-map
         ("/" . /a-forward-slash)
         ;; :map isearch-mode-map
         ;; ("C-w" . nil)
         :map minibuffer-local-map
         ("C-w" . backward-kill-word))
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (fset 'evil-visual-update-x-selection 'ignore)
  (setq evil-want-Y-yank-to-eol t
        sentence-end-double-space nil
        evil-regexp-search t
        evil-normal-state-modes (append evil-motion-state-modes
                                        evil-normal-state-modes)
        evil-motion-state-modes nil
        evil-want-C-u-scroll t
        evil-split-window-below t
        evil-vsplit-window-right t)

  (add-hook 'view-mode-hook 'evil-motion-state)

  (evil-define-text-object /a-forward-slash (count &optional beg end type)
    "Select forward slash (/)"
    :extend-selection t
    (evil-select-quote ?/ beg end type count))

  (evil-define-text-object /inner-forward-slash (count &optional beg end type)
    "Select forward slash (/)"
    :extend-selection nil
    (evil-select-quote ?/ beg end type count))

  (modify-syntax-entry ?_ "w")

  ;; ;; Let `_` be considered part of a word, like vim does
  ;; (defadvice evil-inner-word (around underscore-as-word activate)
  ;;   (let ((table (copy-syntax-table (syntax-table))))
  ;;     (modify-syntax-entry ?_ "w" table)
  ;;     (with-syntax-table table ad-do-it)))

  (evil-ex-define-cmd "tabn[ew]" 'make-frame)
  (evil-ex-define-cmd "tabe[dit]" 'make-frame)

  ;; (lexical-let ((default-color (cons (face-background 'mode-line)
  ;;                                    (face-foreground 'mode-line))))
  ;;   (add-hook 'post-command-hook
  ;;             (lambda ()
  ;;               (let ((color (cond ((minibufferp) default-color)
  ;;                                  ((evil-insert-state-p) '("#b58900" . "#ffffff"))
  ;;                                  ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
  ;;                                  ((buffer-modified-p)   '("#dc322f" . "#ffffff"))
  ;;                                  (t default-color))))
  ;;                 (set-face-background 'mode-line (car color))
  ;;                 (set-face-foreground 'mode-line (cdr color))))))
  )

(use-package evil-leader
  :ensure t
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1)
  )

(use-package evil-embrace
  :ensure t
  :config
  (progn (require 'evil-surround)
         (evil-embrace-enable-evil-surround-integration)
         (setq evil-embrace-show-help-p nil)
         ))

(use-package evil-args
  :ensure t
  :after evil
  :bind (:map evil-inner-text-objects-map
              ("a" . evil-inner-arg)
              :map evil-outer-text-objects-map
              ("a" . evil-outer-arg)
              :map evil-normal-state-map
              ("L" . evil-forward-arg)
              ("H" . evil-backward-arg)
              ("K" . evil-jump-out-args)
              :map evil-motion-state-map
              ("L" . evil-forward-arg)
              ("H" . evil-backward-arg))
  :config
  ;; consider spaces as argument delimiters
  (add-to-list 'evil-args-delimiters " "))

(use-package evil-numbers
  :ensure t
  :after evil
  :bind (:map evil-normal-state-map
              ("C-a" . evil-numbers/inc-at-pt)
              ("C-x" . evil-numbers/dec-at-pt)))

;; alignment
(use-package evil-lion
  :ensure t
  :after evil evil-leader
  :config
  (evil-leader/set-key
    "+" 'evil-lion-left)
  (evil-lion-mode))

(use-package evil-matchit
  :ensure t)

;; (use-package evil-paredit
;;   :config (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode))

;; (use-package evil-cleverparens
;;   :ensure t
;;   :bind(:map evil-inner-text-objects-map
;;              ("c" . evil-cp-inner-comment)
;;              :map evil-outer-text-objects-map
;;              ("c" . evil-cp-a-comment)
;;              )
;;   :config
;;   ;; (progn (require 'evil-cleverparens-text-objects)
;;   ;;        (define-key evil-inner-text-objects-map "c" 'evil-cp-inner-comment)
;;   ;;        (define-key evil-outer-text-objects-map "c" 'evil-cp-a-comment))
;;   (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode 1))

;; (use-package evil-nerd-commenter
;;   :ensure t
;;   :bind(:map evil-inner-text-objects-map
;;              ("c" . evilnc-inner-comment)
;;              :map evil-outer-text-objects-map
;;              ("c" . evilnc-outer-commenter)
;;              ))

;; (use-package evil-replace-with-register)

;; (use-package evil-text-object-python)

(use-package evil-magit
  :ensure t)

(use-package evil-indent-plus
  :ensure t
  :config
  (evil-indent-plus-default-bindings))

;; vim aesthetics
(use-package vi-tilde-fringe
  :ensure t
  :after evil
  :config
  (global-vi-tilde-fringe-mode 1))

(use-package evil-visualstar
  :ensure t
  :after evil
  :config
  (global-evil-visualstar-mode))

;; ;; https://github.com/edkolev/evil-goggles
;; (use-package evil-goggles
;;   :disabled ;; melpa is complaining that they can't find this package
;;   :ensure t
;;   :after evil
;;   :config
;;   (evil-goggles-mode)
;;   (evil-goggles-use-diff-faces)
;;   (setq evil-goggles-duration 0.025))

;; (use-package evil-snipe
;;   :ensure t
;;   :config
;;   (evil-snipe-override-mode))

(use-package evil-quickscope
  :ensure t
  :config
  (global-evil-quickscope-always-mode t)
  ;; (global-evil-quickscope-mode 1)
  )

;; (use-package evil-visual-mark-mode
;;   :ensure t
;;   :config
;;   (evil-visual-mark-mode))

;; (use-package evil-tabs
;;   :ensure t
;;   :config
;;   (global-evil-tabs-mode t))

(use-package vimish-fold
  :ensure t)

(use-package evil-tutor
  :ensure t)

;; activate folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

(evil-mode 1)

(provide 'config-evil)

;;; config-evil.el ends here

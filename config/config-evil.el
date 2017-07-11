;;; config-evil.el --- evil-mode configuration

;;; Commentary:

;;; Code:
(require 'config-package)

;; BEGIN LOCAL FUNCTIONS ---
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
;; END LOCAL FUNCTIONS ---

;; Make my own leader keys and bake it into evil
(defvar /mapleader "<SPC>")

(defun /leader (keystroke)
  "Append our leader key onto KEYSTROKE."
  (concat /mapleader " " keystroke))

;; evil config
(use-package evil
  :ensure t
  :bind (("C-f" . universal-argument)
         ("C-u" . kill-whole-line)
         :map universal-argument-map
         ("C-u" . nil)
         ("C-f" . universal-argument-more)
         :map evil-motion-state-map
         ("C-u" . evil-scroll-up)
         :map evil-normal-state-map
         ("gt" . /evil-gt)
         ("gT" . /evil-gT)
         ("C-\\" . /lang-toggle) ;; binding for eng <-> jap
         :map evil-visual-state-map
         ("p"  . /evil-paste-after-from-0)
         (">>" . /evil-shift-right-visual)
         ("<<" . /evil-shift-left-visual)
         :map minibuffer-local-isearch-map
         ("C-w" . evil-delete-backward-word))
  :config
  (fset 'evil-visual-update-x-selection 'ignore)
  (setq evil-want-Y-yank-to-eol t
        sentence-end-double-space nil
        evil-regexp-search t
        evil-normal-state-modes (append evil-motion-state-modes
                                        evil-normal-state-modes)
        evil-motion-state-modes nil
        evil-want-C-u-scroll t)

  (add-hook 'view-mode-hook 'evil-motion-state)

  ;; Let `_` be considered part of a word, like vim does
  (defadvice evil-inner-word (around underscore-as-word activate)
    (let ((table (copy-syntax-table (syntax-table))))
      (modify-syntax-entry ?_ "w" table)
      (with-syntax-table table ad-do-it)))

  (evil-ex-define-cmd "tabn[ew]" 'make-frame)
  (evil-ex-define-cmd "vsp[lit]" #'(lambda() (interactive)
                                     (split-window-horizontally)
                                     (other-window 1)))
  (evil-ex-define-cmd "sp[lit]" #'(lambda() (interactive)
                                    (split-window-vertically)
                                    (other-window 1)))

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

;; TODO: roll your own evil-leader. This gets annoying after a while
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
  (setq-default evil-surround-pairs-alist
                (cons '(?~ . ("~" . "~"))
                      evil-surround-pairs-alist)))

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
              ("H" . evil-backward-arg)))

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
  :ensure t
  :after evil)

;; (use-package evil-paredit
;;   :config (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode))

;; (use-package evil-cleverparens)

(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode 1))

;; (use-package evil-replace-with-register)

;; (use-package evil-text-object-python)

;; (use-package evil-magit)

(use-package evil-indent-textobject
  :ensure t
  :after evil)

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

;; https://github.com/edkolev/evil-goggles
(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package vimish-fold
  :ensure t)

;; (evil-define-command my-evil-helm-apropos(arg)
;;   (interactive "<a>")
;;   (helm-apropos arg)
;;   (other-window 1)
;;   )

;; (eval-after-load 'evil-maps '(progn (evil-ex-define-cmd "h[elp]" 'my-evil-helm-apropos)))

(evil-mode 1)

(provide 'config-evil)

;;; config-evil.el ends here

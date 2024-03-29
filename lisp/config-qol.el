;;; config-qol.el --- Packages that add quality of life to existing emacs packages

(require 'config-emacs)

(use-package fancy-compilation
  :after compile
  :custom
  (fancy-compilation-override-colors nil)
  (fancy-compilation-scroll-output 'first-error)
  (fancy-compilation-quiet-prolog nil)
  (fancy-compilation-quiet-prelude nil)
  :config
  (fancy-compilation-mode))

(use-package which-key
  :ensure (:host github :repo "justbur/emacs-which-key")
  :demand t
  :init
  (with-eval-after-load 'evil
    (customize-set-value 'which-key-allow-evil-operators t)
    ;; note that this is marked as *experimental*
    (customize-set-value 'which-key-show-operator-state-maps t))
  :custom
  (which-key-use-C-h-commands nil)
  (which-key-is-verbose t)
  (which-key-popup-type 'minibuffer)
  (which-key-side-window-max-width 0.33)
  (which-key-max-display-columns nil)
  (which-key-show-docstrings t)
  (which-key-side-window-location 'bottom)
  ;; :general
  ;; (:states 'normal
  ;;  "C-h M-k" 'which-key-show-keymap
  ;;  "C-h M-m" 'which-key-show-full-major-mode)
  :config
  (which-key-mode))

(use-package smartparens
  :ensure (:host github :repo "Fuco1/smartparens")
  :demand t
  :blackout t
  :commands (sp-local-pair
             smartparens-global-mode)
  :hook
  ;; TODO: make this not just hooked on prog-mode
  (prog-mode-hook . (lambda () (interactive)
                      (require 'smartparens-config) ;; load some default configurations
                      (require 'smartparens)))
  :custom-face
  (sp-pair-overlay-face ((t (:inherit default :underline nil))))
  :general
  (:states 'normal
   :prefix my-default-evil-leader-key
   "." 'smartparens-mode)
  :custom
  (sp-cancel-autoskip-on-backward-movement
   nil
   "We want to maintain the chomp-like behavior of electric-pair")
  (sp-autoskip-closing-pair
   'always
   "Maintain chomp-like behavior of electric-pair")
  :config
  (smartparens-global-mode)
  (cl-defun --double-newline-and-indent-braces (_opening_delimiter
                                                _action
                                                _context)
    "adds that cool vim indent thing we always wanted, Refer to WHEN
      segment of `sp-pair' documentation on what each parameter does"
    (newline)
    (indent-according-to-mode)
    (previous-line)
    (indent-according-to-mode))
  (sp-local-pair 'prog-mode "{" nil :post-handlers '((--double-newline-and-indent-braces "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((--double-newline-and-indent-braces "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((--double-newline-and-indent-braces "RET"))))

(use-package hl-todo
  :disabled t
  :ensure (:host github :repo "tarsius/hl-todo")
  :blackout t
  :commands (hl-todo-mode)
  :hook ((prog-mode-hook  . hl-todo-mode)
         (yaml-mode-hook  . hl-todo-mode))
  :custom
  (hl-todo-keyword-faces '(("TODO"  . "#b58900")
                           ("DEBUG" . "#d33682")
                           ("NOTE"  . "#586e75")
                           ("FIXME" . "#cb4b16")))
  :general
  ;; (:states 'normal
  ;;  :prefix my-default-evil-leader-key
  ;;  "t t" 'my-helm-swoop-hl-todo)
  (:keymaps 'evil-normal-state-map
   "[ h"  'hl-todo-previous
   "] h"  'hl-todo-next))
;; :init
;;TODO: Make this search for regexes
;; (defun my-helm-swoop-hl-todo () (interactive)
;;        (require 'helm-swoop)
;;        (helm-swoop :$query hl-todo-regexp :$multiline 4)))
;; Stolen from https://github.com/emacs-helm/helm/wiki/Developing. Convenient!
;; Not used because we don't incrementally search for todos
;; (defun my-helm-hl-todo-items ()
;;   "Show `hl-todo'-keyword items in buffer."
;;   (interactive)
;;   (hl-todo--setup)
;;   (helm :sources (helm-build-in-buffer-source "hl-todo items"
;;                    :data (current-buffer)
;;                    :candidate-transformer (lambda (candidates)
;;                                             (cl-loop for c in candidates
;;                                                      when (string-match hl-todo--regexp c)
;;                                                      collect c))
;;                    :get-line #'buffer-substring)
;;         :buffer "*helm hl-todo*"))

;; TODO: update `dired-collapse--create-ov' to not hardcode the shadow
;; face. This should be a PR
(use-package dired-hacks
  :after dired
  :ensure (:host github :repo "Fuco1/dired-hacks")
  ;; :general
  ;; (dired-mode-map
  ;;  :states 'normal
  ;;   "TAB" 'dired-subtree-toggle
  ;;   "z a" 'dired-subtree-toggle
  ;;   "z o" 'dired-subtree-insert
  ;;   "z c" 'dired-subtree-remove)
  :hook ((dired-mode-hook . dired-collapse-mode)
         (dired-mode-hook . dired-filter-mode)))

(use-package dumb-jump
  :ensure (:host github :repo "jacktasia/dumb-jump")
  :config
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package helpful
  :ensure (:host github :repo "Wilfred/helpful")
  :general
  ("C-h k"   'helpful-key
   "C-h f"   'helpful-callable
   "C-h x"   'helpful-command
   "C-h v"   'helpful-variable
   "C-h o"   'helpful-symbol
   "C-h RET" 'helpful-at-point
   "C-h M-k" '--helpful-keymap)
  :init
  (defun --helpful-keymap ()
    (interactive)
    (require 'helpful)
    (let ((sym (--completing-read "Keymap: " obarray
                                  :predicate #'(lambda (sym)
                                                 (and (boundp sym)
                                                      (keymapp (symbol-value sym))))
                                  :require-match t)))
      (-> sym (intern) (helpful-symbol))))
  :config
  (require 'link-hint)
  (general-define-key
   :keymaps 'helpful-mode-map
   :states 'normal
   "f" 'link-hint-open-link
   "y f" 'link-hint-copy-link))
  ;; (helpful-mode-map
  ;;  :states 'normal
  ;;  "f" 'ace-link-help
  ;;  "F" 'ace-link-help))

(use-package elisp-demos
  :after (helpful)
  :ensure (:host github :repo "xuchunyang/elisp-demos"
             :files (:defaults "elisp-demos.org"))
  :config
  (with-eval-after-load 'helpful
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))

(use-package writeroom-mode
  :ensure (:host github :repo "joostkremers/writeroom-mode")
  :commands (writeroom-mode)
  :custom
  (writeroom-mode-line t)
  (writeroom-bottom-divider-width 0)
  (writeroom-maximize-window nil)
  (writeroom-fullscreen-effect 'maximized)
  (writeroom-width 80)
  (writeroom-restore-window-config nil)
  ;; :general
  ;; (:states 'normal
  ;;  "g z" 'writeroom-mode)
  :init
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "writeroom" 'writeroom-mode)))
;; :hook
;; ((writeroom-mode-hook . (lambda () (require 'focus)
;;                      (if writeroom-mode
;;                          (progn (focus-init)
;;                                 (when display-line-numbers-mode
;;                                   (display-line-numbers-mode -1)))
;;                        (focus-terminate)
;;                        (unless display-line-numbers-mode
;;                          (display-line-numbers-mode)))))))

(provide 'config-qol)

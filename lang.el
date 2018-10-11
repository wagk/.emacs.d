(use-package parinfer
  :straight (:host github :repo "DogLooksGood/parinfer-mode" :branch "master")
  :general
  (parinfer-mode-map
   "\"" nil) ;; let smartparens do its thing
  :custom
  (parinfer-auto-switch-indent-mode t
				    "We prefer indent mode")
  :init
  (progn (setq parinfer-extensions
	       '(defaults       ; should be included.
		  pretty-parens  ; different paren styles for different modes.
		  evil           ; If you use Evil.
		  smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
		  smart-yank))))   ; Yank behavior depend on mode.

(use-package rainbow-delimiters
  :diminish rainbow-delimiters-mode
  :commands (rainbow-delimiters-mode))

(use-package smartparens
  :defer 2
  :straight (:host github :repo "Fuco1/smartparens" :branch "master")
  :diminish smartparens-mode
  :commands (sp-local-pair)
  :general
  (:states 'normal
   :prefix my-default-evil-leader-key
   "." 'smartparens-mode)
  :custom
  (sp-cancel-autoskip-on-backward-movement
   nil "We want to maintain the chomp-like behavior of electric-pair")
  (sp-autoskip-closing-pair
   'always "Maintain chomp-like behavior of electric-pair")
  :config
  (require 'smartparens-config) ;; load some default configurations
  (smartparens-global-mode)
  ;;(smartparens-global-strict-mode)
  ;;(show-smartparens-global-mode)
  ;; define some helper functions
  (defun my-add-newline-and-indent-braces (&rest _)
    "Adds that cool vim indent thing we always wanted"
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  ;; Update the global definitions with some indenting
  ;; I think that the nil is the flag that controls property inheritance
  ;;NOTE: For some reason TAB isn't recognised. Might be yasnippet intefering.
  ;;Learn to use ret for now
  (sp-pair "{" nil :post-handlers '((my-add-newline-and-indent-braces "RET")))
  (sp-pair "[" nil :post-handlers '((my-add-newline-and-indent-braces "RET")))
  (sp-pair "(" nil :post-handlers '((my-add-newline-and-indent-braces "RET"))))

(use-package elisp-mode
  :ensure nil
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
	 (emacs-lisp-mode . parinfer-mode)
	 (emacs-lisp-mode . update-evil-shift-width)))

;;; config-qol.el --- Packages that add quality of life to existing emacs packages

(require 'config-emacs)

(use-package context-transient
  :ensure (:host github :repo "licht1stein/context-transient.el")
  :after transient
  :commands context-transient
  :general
  (:states 'normal
   "|" #'context-transient))

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
  (which-key-show-remaining-keys t)
  (which-key-show-prefix 'top)
  (which-key-side-window-location 'bottom)
  ;; :general
  ;; (:states 'normal
  ;;  "C-h M-k" 'which-key-show-keymap
  ;;  "C-h M-m" 'which-key-show-full-major-mode)
  :config
  (which-key-mode))

(use-package hl-todo
  :disabled t
  ;; :ensure (:host github :repo "tarsius/hl-todo")
  :ensure t
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
  (writeroom-width 100)
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

(use-package rainbow-mode)

;; whitespace
(use-package ws-butler
  :ensure t
  :custom
  (ws-butler-global-exempt-modes '())
  :config
  (ws-butler-global-mode))

(use-package flycheck
  :ensure (:host github :repo "flycheck/flycheck")
  :custom
  (flycheck-indication-mode 'left-margin)
  ;; :custom-face
  ;; (flycheck-error ((t (:underline (:color ,sol-red :style line)))))
  ;; (flycheck-delimited-error ((t (:inherit flycheck-error))))
  ;; (flycheck-info ((t (:underline (:color ,sol-blue :style line)))))
  ;; (flycheck-warning ((t (:underline (:color ,sol-yellow :style line)))))
  ;; (flycheck-fringe-error ((((background light)) (:background ,sol-base3 :foreground ,sol-red))
  ;;                         (((background dark)) (:background ,sol-base03 :foreground ,sol-red))))
  ;; (flycheck-fringe-info ((((background light)) (:background ,sol-base3 :foreground ,sol-blue))
  ;;                        (((background dark)) (:background ,sol-base03 :foreground ,sol-blue))))
  ;; (flycheck-fringe-warning ((((background light)) (:background ,sol-base3 :foreground ,sol-yellow))
  ;;                           (((background dark)) (:background ,sol-base03 :foreground ,sol-yellow))))
  :hook
  (prog-mode-hook . global-flycheck-mode)
  :config
  (define-advice flycheck-mode-line-status-text
      (:filter-return (text) --flycheck-mute-modeline-colors)
    "Make the modeline coloring of flycheck warnings/errors less striking."
    (put-text-property 0 (length text) 'face 'nano-default text)
    text))

(use-package engine-mode
  :commands defengine
  :after (evil general)
  :init
  (evil-define-command --ddg-search (beg end _type)
    (interactive "<v>")
    (require 'engine-mode)
    (unless (boundp 'engine/search-duckduckgo)
      (defengine duckduckgo
        "https://duckduckgo.com/?q=%s"))
    (let* ((query-region (when (use-region-p)
                           (buffer-substring beg end)))
           (query-params (when (evil-ex-p)
                           evil-ex-argument))
           (query-args (list query-params query-region))
           (query (if (-none-p 'identity query-args)
                      (read-string "Search: " nil nil
                                   (thing-at-point 'word))
                    (s-join " " query-args))))
      (engine/search-duckduckgo query)))
  ;; (evil-ex-define-cmd "ddg" '--ddg-search)
  :general
  (:states '(normal motion visual)
   "K" '--ddg-search))

(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default)
  (with-eval-after-load 'context-transient
    (context-transient-define scratch
      :doc "Scratch buffer operations."
      :buffer "*scratch*"
      :menu
      ["Persistent Scratch"
       [("w" "Write" persistent-scratch-save)
        ("e" "Read" persistent-scratch-restore)
        ("n" "New buffer" persistent-scratch-new-backup)]
       [("f w" "Write to file" persistent-scratch-save-to-file)
        ("f e" "Read from file" persistent-scratch-restore-from-file)]])))


(use-package link-hint
  :after general
  :commands (link-hint-open-link
             link-hint-open-link-at-point
             link-hint-copy-link
             link-hint-copy-link-at-point)
  ;; :custom
  ;; (link-hint-action-fallback-commands
  ;;  (list :open
  ;;    (lambda ()
  ;;      (cond
  ;;       ((eq last-command 'link-hint-open-link-at-point) (link-hint-open-link))
  ;;       ((eq last-command 'link-hint-copy-link-at-point) (link-hint-copy-link))))))
  :general
  (:keymaps 'help-mode-map
   :states '(motion normal)
   "f" 'link-hint-open-link
   "y f" 'link-hint-copy-link)
  :config
  (cl-defun --link-hint-open-link-fallback (&rest _)
    (condition-case nil
        (apply #'link-hint-open-link-at-point)
      (error (link-hint-open-link))))
  (advice-add 'link-hint-open-link-at-point :around
              #'--link-hint-open-link-fallback)
  (cl-defun --link-hint-copy-link-fallback (&rest _)
    (condition-case nil
        (apply #'link-hint-copy-link-at-point)
      (error (link-hint-copy-link))))
  (advice-add 'link-hint-copy-link-at-point :around
              #'--link-hint-copy-link-fallback))


(use-package highlight-indent-guides
  :commands (highlight-indent-guides-mode)
  :after general
  :general
  (:states 'normal
   :prefix my-default-evil-leader-key
   "|" #'(lambda () (interactive)
           (require 'display-fill-column-indicator)
           (require 'highlight-indent-guides)
           (display-fill-column-indicator-mode 'toggle)
           (highlight-indent-guides-mode 'toggle)))
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-auto-enabled nil)
  :hook
  ((prog-mode-hook . highlight-indent-guides-mode)))

(use-package fill-function-arguments
  :ensure (:host github :repo "davidshepherd7/fill-function-arguments")
  :after general
  :commands (fill-function-arguments-dwim)
  :custom (fill-function-arguments-indent-after-fill t)
  :general
  (:states 'normal
   "g *" 'fill-function-arguments-dwim))

(use-package macrostep
  :commands (macrostep-expand))

(use-package eros
  :custom-face
  (eros-result-overlay-face ((t (:box nil
                                 :background unspecified
                                 :inherit shadow))))
  :config
  (eros-mode))

;; https://github.com/Silex/docker.el
(use-package docker
  :commands docker
  :after evil
  :init
  (evil-ex-define-cmd "docker" #'docker))

(use-package svg-lib
  :ensure (:host github :repo "rougier/svg-lib"))

(use-package svg-tag-mode
  :after svg-lib)

(use-package multi-term
  :if (not (or (featurep 'vterm)
               (featurep 'multi-vterm)
               (eq system-type 'windows-nt)))
  :commands
  (multi-term))

;; https://github.com/akermu/emacs-libvterm
;;
;; Ensure that `libtool` is installed. On Ubuntu this can be done via
;; `libtool-bin`.
(use-package vterm
  :if (not (eq system-type 'windows-nt))
  :after evil
  :custom
  (vterm-max-scrollback 100000 "maximum allowed without editing source file.")
  (vterm-always-compile-module t)
  :config
  (evil-ex-define-cmd "term" #'vterm))

(use-package multi-vterm
  :after (vterm general evil)
  :if (not (eq system-type 'windows-nt))
  :general
  (general-define-key
   :keymaps 'project-prefix-map
   "s" 'multi-vterm-project) ;; overrides `project-shell'
  :config
  (evil-ex-define-cmd "term" #'multi-vterm))

(with-eval-after-load 'evil
  (when (fboundp 'shortdoc)
   (evil-ex-define-cmd "cheatsheet" 'shortdoc)
   (evil-ex-define-cmd "cs" 'shortdoc)))

(use-package dwim-shell-command
  :ensure (:host github :repo "xenodium/dwim-shell-command"))

(use-package scopeline
  :ensure (:host github :repo "meain/scopeline.el" :branch "master")
  :after (:or tree-sitter treesit)
  :hook (prog-mode-hook . scopeline-mode)
  :blackout t
  :custom
  (scopeline-overlay-prefix " -- ")
  (scopeline-min-lines 10))

;; Don't forget to set =chatgpt-shell-openai-key= somewhere.
(use-package chatgpt-shell
  :disabled t
  :ensure (:host github :repo "xenodium/chatgpt-shell")
  :after (dash evil)
  :commands chatgpt-shell-prompt
  :general
  (chatgpt-shell-mode-map
   "RET" nil
   "C-<return>" 'comint-send-input)
  :custom
  (chatgpt-shell-request-timeout 300)
  (chatgpt-shell-insert-queries-inline nil)
  (chatgpt-shell-history-path no-littering-var-directory)
  (chatgpt-shell-display-function
   #'(lambda (buf)
       (pop-to-buffer buf #'display-buffer-pop-up-window)))
  (chatgpt-shell-prompt-query-response-style 'shell)
  :hook
  (chatgpt-shell-mode-hook . (lambda ()
                               (when (bound-and-true-p parinfer-rust-mode)
                                 (parinfer-rust-mode 0))))
  :config
  (setq chatgpt-shell-system-prompt
        (-find-index (lambda (elem)
                       (string-equal "Programming" (car elem)))
                     chatgpt-shell-system-prompts))
  (setq chatgpt-shell-model-version
        (-find-index (lambda (elem)
                       (string-equal "gpt-4-turbo-preview" elem))
                     chatgpt-shell-model-versions))
  (setq chatgpt-shell-welcome-function
        #'(lambda (config)
            "Adapted from `shell-maker-welcome-function'"
           (format
            "Welcome to %s shell\n\n  Type %s and press %s for details.\n\n"
            (propertize (shell-maker-config-name config)
                        'font-lock-face 'font-lock-comment-face)
            (propertize "help" 'font-lock-face 'italic)
            (shell-maker--propertize-key-binding "-shell-submit" config))))
  :init
  (evil-define-command --evil-gpt-cmd (prompt)
    (interactive "<a>")
    (require 'chatgpt-shell)
    (if prompt
        (chatgpt-shell-send-to-buffer prompt)
      (chatgpt-shell)))
  (evil-ex-define-cmd "gpt" #'--evil-gpt-cmd))

(use-package exec-path-from-shell
  :if (not (eq system-type 'windows-nt))
  :config
  (exec-path-from-shell-initialize))

(use-package sudo-edit
  :if (not (eq system-type 'windows-nt))
  :commands sudo-edit)

(use-package gptel
  :ensure t
  :after (general evil config-evil-helpers)
  :custom
  ;; prompts are under `gptel-directives'
  ;; (gptel-model "gpt-4-1106-preview")
  (gptel-display-buffer-action '(display-buffer-same-window))
  :general
  (gptel-mode-map
   "C-<return>" #'(lambda () (interactive)
                    (goto-char (point-max))
                    (gptel-send))
   "C-c RET" nil)
  :hook
  (gptel-mode-hook . (lambda ()
                       (require 'visual-fill-column)
                       (visual-line-fill-column-mode 1)))
  :init
  (evil-ex-define-cmd "gptt" #'gptel-menu)
  (--evil-define-splits "gpt" 'gptel))
  ;; :config
  ;; (define-advice gptel (:filter-args (args) --lint-dynamic-buffer-name)
  ;;   "Refer to `gptel' for argument list."
  ;;   (require 's)
  ;;   (pcase-let ((`(,name ,a ,b ,c) args))
  ;;     (unless (s-contains-p (format "<")))
  ;;     (setq name (format "*<%s>%s*" gptel-model name))
  ;;     (list name a b c))))

;; https://github.com/jorgenschaefer/emacs-buttercup/blob/master/docs/writing-tests.md
;; https://github.com/jorgenschaefer/emacs-buttercup/blob/master/docs/running-tests.md
(use-package buttercup)

;; Debug printf statements
(use-package ll-debug
  :disabled t
  :commands ll-debug-insert
  :after evil
  :custom
  (ll-debug-output-prefix "DEBUG-pangt-"))
  ;; :init
  ;; (evil-ex-define-cmd "di" #'ll-debug-insert))

(use-package pomm
  :ensure t
  :after evil
  :commands (pomm
             pomm-start-with-context
             pomm-third-time
             pomm-third-time-start-with-context)
  :init
  (evil-ex-define-cmd "timer" 'pomm))

(use-package restart-emacs
  :if (not (eq system-type 'darwin))
  :ensure (:host github :repo "iqbalansari/restart-emacs")
  :commands (restart-emacs restart-emacs-start-new-emacs)
  :after evil
  :init
  (evil-ex-define-cmd "restart" 'restart-emacs)
  (evil-ex-define-cmd "restarttest" 'restart-emacs-start-new-emacs))

(use-package eat
  :ensure (:type git
           :host codeberg
           :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el")))
  :custom
  (eat-term-name "*eat-term*"))

(provide 'config-qol)

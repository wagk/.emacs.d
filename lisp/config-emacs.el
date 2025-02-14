;;; Configuration for packages built into Emacs

(require 'use-package)
(require 'config-evil)

;; Catch-all config for emacs variables
(use-package emacs
  :demand t
  :ensure nil
  :custom
  (confirm-kill-emacs nil)
  (confirm-kill-processes nil)
  (cursor-in-non-selected-windows nil)
  (history-delete-duplicates t)
  (history-length t)
  (version-control t)
  (delete-old-versions t)
  (backup-by-copying t)
  (vc-make-backup-files t)
  (kept-new-versions 20)
  (kept-old-versions 5)
  (ff-always-try-to-create nil)
  (delete-by-moving-to-trash t)
  (blink-matching-paren-highlight-offscreen t)
  (truncate-lines t)
  (inhibit-startup-screen t)
  (require-final-newline t)
  (ring-bell-function 'ignore)
  (tab-width 4)
  (sentence-end-double-space nil)
  (x-stretch-cursor t) ;; http://pragmaticemacs.com/emacs/adaptive-cursor-width/
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (show-paren-context-when-offscreen t)
  (tags-add-tables nil)
  (use-dialog-box nil)
  ;; (fill-column 80)
  (frame-resize-pixelwise t)
  (inhibit-compacting-font-caches t)
  ;; make things more vim-like
  (scroll-step 1)
  (scroll-margin 1)
  (scroll-conservatively 9999)
  (whitespace-line-column nil)
  :init
  ;; stolen from
  ;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/#h:1e468b2a-9bee-4571-8454-e3f5462d9321
  (cl-defun --keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
    (interactive)
    (cond
     ((region-active-p)
      (keyboard-quit))
     ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
     ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
     (t
      (keyboard-quit))))
  :general
  ("C-g" #'--keyboard-quit-dwim)
  (:states 'normal
   :prefix my-default-evil-leader-key
   "." 'whitespace-mode)
  (:keymaps 'Buffer-menu-mode-map
   :states '(normal motion)
   "C-d" 'evil-scroll-down) ;; replaces `Buffer-menu-delete-backwards`
  (:keymaps 'esc-map
   ":" nil ;; otherwise ESC : runs `eval-expression' instead of evil-ex
   "C-w" nil)
  :hook
  (prog-mode-hook . hs-minor-mode)
  (prog-mode-hook . show-paren-mode)
  (prog-mode-hook . (lambda () (setq-local show-trailing-whitespace t)))
  (after-make-frame-functions . select-frame)
  (after-change-major-mode-hook . (lambda () (modify-syntax-entry ?_ "w")))
  :config
  (electric-indent-mode)

  (when (>= emacs-major-version 26)
    (add-hook 'prog-mode-hook 'display-line-numbers-mode))

  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (when (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (window-divider-mode -1)

  (when (boundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode))

  (save-place-mode 1)

  (column-number-mode 1)
  (setq-default indent-tabs-mode nil)

  (if (version<= emacs-version "28")
      (fset 'yes-or-no-p 'y-or-n-p)
    (setopt use-short-answers t))

  (unless (featurep 'no-littering)
    (setq backup-directory-alist
          `(("." .  ,(expand-file-name
                      (file-name-concat user-emacs-directory "backups"))))))

  (when (< emacs-major-version 27)
    (setq w32-pipe-read-delay 0)))

;; Note that while the buffer will look rot13 encrypted, the final saved) file
;; will not itself be encrypted.)
(with-eval-after-load 'evil
  (evil-ex-define-cmd "rot[13]" 'toggle-rot13-mode))

(use-package faces
  :ensure nil
  :after config-theme
  :custom-face
  (trailing-whitespace
   ((default . (:foreground unspecified
                :inverse-video nil
                :inherit sol-superlight-background))))
  (warning
   ((default . (:foreground ,sol-orange))))
  (success
   ((default . (:foreground ,sol-green))))
  (error
   ((default . (:foreground unspecified
                :bold t
                :background ,sol-red
                :inverse-video nil
                :inherit sol-foreground-i
                :background ,sol-red))
    (((supports (:bold)))) . (:bold t)))
  (minibuffer-prompt
   ((default . (:foreground ,sol-green))))
  (shadow
   ((default . (:foreground unspecified
                :inherit sol-light-foreground))))
  (highlight
   ((default . (:background unspecified
                :inherit sol-superlight-background))))
  (link-visited
   ((default . (:foreground ,sol-blue))))
  (region
   ((default . (:foreground unspecified
                :background unspecified
                :inverse-video t
                :inherit sol-superlight-background))))
  (fringe
   ((default . (:foreground unspecified
                :background unspecified
                :inherit sol-superlight-foreground))))
  (variable-pitch-text
   ((((supports (:height))) . (:height 1))))
  (fill-column-indicator
   ((default . (:inherit sol-superlight-foreground))))
  (show-paren-match
   ((default . (:foreground unspecified
                :background unspecified
                :inherit sol-superlight-background))
    (((supports (:bold))) . (:bold t))))
  (show-paren-mismatch
   ((default . (:background ,sol-red
                :foreground unspecified
                :inherit sol-foreground-i))))
  (vertical-border
   ((default . (:foreground unspecified
                :inherit sol-superlight-foreground))))
  (mode-line
   ((default . (:foreground unspecified
                :background unspecified
                :inverse-video nil
                :inherit sol-superlight-box))))
  (mode-line-active
   ((default . (:inherit (sol-light-foreground
                          sol-superlight-box)))))
  (mode-line-emphasis
   ((default . (:inherit sol-light-foreground))))
  (mode-line-highlight
   ((t . (:inherit sol-light-foreground))))
  (mode-line-buffer-id
   ((default . (:box unspecified
                :inherit sol-light-foreground))))
  (mode-line-inactive
   ((default . (:inverse-video nil
                :foreground unspecified
                :background unspecified
                :overline unspecified
                :underline unspecified
                :box unspecified
                :inherit (sol-superlight-box
                          sol-superlight-foreground)))))
  (header-line
   ((default . (:underline nil
                :inverse-video nil
                :foreground unspecified
                :background unspecified))))
  (header-line-active
   ((default . (:inherit header-line))))
  (help-key-binding
   ((default . (:foreground ,sol-yellow
                :box nil
                :background unspecified))))
  (link
   ((default . (:foreground ,sol-yellow
                :underline t
                :bold nil))))
  (line-number
   ((default . (:inherit sol-superlight-foreground))))
  (line-number-current-line
   ((default . (:inherit sol-light-foreground))))
  (window-divider
   ((default . (:foreground unspecified
                :background unspecified)))))

(use-package minibuffer
  :ensure nil
  :after config-theme
  :custom
  (enable-recursive-minibuffers t)
  :custom-face
  (completions-common-part ((default . (:foreground ,sol-cyan)))))

(use-package time
  :ensure nil
  :custom
  (display-time-format "%F %a %H:%M %z")
  :config
  ;; remove the `load' entry, which displays system load.
  (setq display-time-string-forms (delq 'load display-time-string-forms))
  (display-time-mode))

(use-package peg
  :ensure (:host github :repo "emacs-straight/peg" :branch "master"))

(use-package font-lock
  :ensure nil
  :after config-theme
  :custom-face
  (font-lock-warning-face
   ((default . (:foreground ,sol-blue))
    ;; fallback to error
    (((type nil)) . (:foreground unspecified))
    (((supports (:bold)) (supports (:italic))) .
     (:bold t :italic t))))
  (font-lock-keyword-face
   ((default . (:foreground unspecified))))
  (font-lock-constant-face
   ((default . (:foreground unspecified))))
  (font-lock-function-name-face
   ((default . (:foreground unspecified))))
  (font-lock-builtin-face
   ((default . (:foreground unspecified))))
  (font-lock-variable-name-face
   ((default . (:foreground unspecified
                :inherit sol-superstrong-foreground))
    ;; if character terminal, use colors
    (((type nil)) . (:inherit sol-strong-foreground))
    ;; otherwise make it pop by bolding
    (t . (:bold t :inherit sol-foreground))))
  (font-lock-comment-face
   ((default . (:italic t
                :foreground unspecified
                :inherit (sol-light-foreground sol-superlight-background)))
    (((type nil)) . (:inherit (sol-superlight-background
                               sol-light-foreground)))
    (((supports (:weight))) . (:weight semi-light))))
  (font-lock-comment-delimiter-face
   ((default . (:inherit font-lock-comment-face))))
  (font-lock-doc-face
   ((default . (:extend t
                :italic nil
                :inherit font-lock-comment-face))))
  (font-lock-type-face
   ((default . (:foreground unspecified
                :inherit sol-foreground))
    (((supports (:italic))) . (:italic t))))
  (font-lock-preprocessor-face
   ((default . (:foreground unspecified
                :inherit sol-foreground))))
  (font-lock-string-face
   ((default . (:foreground unspecified
                :inherit sol-foreground)))))

(use-package savehist
  :ensure nil
  :custom
  (savehist-additional-variables
   '(kill-ring search-ring regexp-search-ring))
  :hook
  (after-init-hook . savehist-mode))

(use-package hideshow
  :ensure nil
  :blackout hs-minor-mode)

(use-package display-fill-column-indicator
  :ensure nil
  :if (>= emacs-major-version 27)
  :custom
  (display-fill-column-indicator-character nil)
  :config
  (global-display-fill-column-indicator-mode))

(cl-defun --kill-buffer-path ()
  (interactive)
  (let ((name (--find-buffer-path)))
    (if (not name)
        (user-error "Buffer not associated with any path.")
      (kill-new name)
      (message "%s" name))))

(cl-defun --point-to-file-and-line-number ()
  (interactive)
  (let* ((buf (--find-buffer-path))
         (lines (if-let* ((_ (use-region-p))
                          (beg-line (save-excursion
                                      (goto-char (use-region-beginning))
                                      (line-number-at-pos)))
                          (end-line (save-excursion
                                      (goto-char (max (- (use-region-end) 1)
                                                      (use-region-beginning)))
                                      (line-number-at-pos)))
                          (_ (not (eq beg-line end-line))))
                    (concat (number-to-string beg-line) "-"
                            (number-to-string end-line))
                  (number-to-string (line-number-at-pos))))
         (name (concat buf ":" lines)))
    (if (not name)
        (user-error "Buffer not associated with any path.")
      (kill-new name)
      (message "%s" name))))

(cl-defun --find-buffer-path ()
  "Tries to find the path of this buffer.
If inside a project, make relative to project root.
Returns a string, or nil if there is no path associated with the buffer."
  (require 'project)
  (require 'dash)
  (let ((name (or (buffer-file-name) dired-directory)))
    (when (project-current)
      (setq name (--> name
                      (file-relative-name it (project-root (project-current)))
                      (file-name-concat
                       (concat "<" (project-name (project-current)) ">") it))))
    (pcase name
      ;; hack since dired-directory might be a list
      ((or 'nil (pred listp)) nil)
      (_ name))))

(with-eval-after-load 'evil
  ;; buffer level info yanking
  (evil-ex-define-cmd "byl" #'--point-to-file-and-line-number)
  (evil-ex-define-cmd "yl" "byl")
  (evil-ex-define-cmd "byf" #'--kill-buffer-path)
  (evil-ex-define-cmd "yf" "byf"))

;; from https://github.com/bbatsov/crux
(cl-defun --eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (backward-kill-sexp)
    (insert (format "%S" value))))

(with-eval-after-load 'evil
  (evil-ex-define-cmd "eval" #'--eval-and-replace))

(with-eval-after-load 'minibuffer
  (general-define-key
   :keymap 'minibuffer-mode-map
   "C-<return>" "RET"))

;; lisp stolen from https://github.com/progfolio/elpaca/issues/216
(cl-defun --elpaca-unload-track-changes (e)
  (and (featurep 'track-changes) (unload-feature 'track-changes t))
  (elpaca--continue-build e))

(cl-defun --elpaca-build-track-changes ()
  (append (butlast (if (file-exists-p (expand-file-name "track-changes" elpaca-builds-directory))
                       elpaca--pre-built-steps
                     elpaca-build-steps))
          (list #'--elpaca-unload-track-changes #'elpaca--activate-package)))

(use-package track-changes
  :ensure `(track-changes
            :build ,(--elpaca-build-track-changes)
            :host github
            :repo "emacs-straight/track-changes"
            :branch "master")
  :demand t)

(use-package eww
  :ensure nil
  :commands eww
  :after config-theme
  :custom-face
  (eww-form-submit
   ((default . (:weight bold
                :inherit sol-light-background-i))))
  (eww-form-textarea
   ((default . (:inherit sol-foreground))))
  (eww-form-text
   ((default . (:inherit sol-light-foreground))))
  (eww-form-select
   ((default . (:foreground ,sol-green))))
  (eww-form-file
   ((default . (:inherit sol-foreground))))
  (eww-form-checkbox
   ((default . (:inherit sol-light-foreground
                :box t)))))

(use-package replace
  :ensure nil
  :after config-theme
  :custom-face
  (match
   ((default (:background unspecified
              :distant-foreground unspecified
              :inverse-video nil
              :foreground ,sol-orange))
    (((type nil)) (:inverse-video t)))))

(use-package re-builder
  :ensure nil
  :after config-theme
  :custom-face
  (reb-match-0
   ((default . (:foreground ,sol-cyan))))
  (reb-match-1
   ((default . (:foreground ,sol-blue))))
  (reb-match-2
   ((default . (:foreground ,sol-violet))))
  (reb-match-3
   ((default . (:foreground ,sol-magenta)))))

(use-package table
  :ensure nil
  :after config-theme
  :custom-face
  (table-cell
   ((default . (:inherit sol-foreground)))))

(use-package sh-script
  :ensure nil
  :commands shell-script-mode
  :after config-theme
  :custom-face
  (sh-heredoc
   ((default . (:foreground ,sol-green))))
  (sh-quoted-exec
   ((default . (:foreground ,sol-green)))))

(use-package queue
  :ensure (:host github :repo "emacs-straight/queue" :branch "master"))

(use-package ielm
  :ensure nil
  :commands ielm
  :init
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "repl" 'ielm)))

(use-package dired
  :ensure nil
  :after config-theme
  :hook
  (dired-mode-hook . hl-line-mode)
  (dired-mode-hook . dired-async-mode)
  (dired-mode-hook . dired-hide-details-mode)
  :custom
  ;; (dired-create-destination-dirs-on-trailing-dirsep nil)
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t)
  :custom-face
  (dired-directory
   ((default . (:bold t :inherit sol-foreground))
    (((type nil)) . (:inherit sol-strong-foreground))))
  (dired-broken-symlink
   ((default . (:background ,sol-red
                :foreground unspecified
                :inherit sol-foreground-i))))
  :general
  (dired-mode-map
   :states 'normal
   "-" 'dired-up-directory
   "d" 'dired-create-directory
   "Y" #'(lambda () (interactive)
           (dired-copy-filename-as-kill 0)))
  (dired-mode-map
   :states 'normal
   :prefix my-default-evil-leader-key
   "<SPC>" 'execute-extended-command)
  :config
  (cl-defun --dired-disable-space ()
    (general-define-key
     :keymaps 'dired-mode-map
     :states 'normal
      "<SPC>" nil ;; was shadowing leader key bindings
      "SPC" nil)) ;; was shadowing leader key bindings
  ;; run this after evil-collections
  (add-hook 'elpaca-after-init-hook #'--dired-disable-space)
  (with-eval-after-load 'blackout
    (with-eval-after-load 'dired-async
      (blackout 'dired-async-mode))
    (with-eval-after-load 'dired-filter
      (blackout 'dired-filter-mode)))
  :init
  (with-eval-after-load 'dired-aux
    (general-define-key
     :keymaps 'dired-mode-map
     "C-=" #'dired-create-empty-file))
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "Ex[plore]" 'dired-jump)
    (evil-ex-define-cmd "Sex[plore]" #'(lambda () (interactive)
                                         (call-interactively 'evil-window-split)
                                         (dired-jump)))
    (evil-ex-define-cmd "Vex[plore]" #'(lambda () (interactive)
                                         (call-interactively 'evil-window-vsplit)
                                         (dired-jump)))
    (evil-ex-define-cmd "Tex[plore]" #'(lambda () (interactive)
                                         (if (>= emacs-major-version 27)
                                             (tab-bar-new-tab)
                                           (my-evil-new-tab nil))
                                         (dired-jump)))))

(use-package outline
  :ensure nil
  :after (config-theme)
  :custom-face
  (outline-1
   ((default . (:bold t
                :foreground unspecified
                :inherit sol-strong-foreground))))
  (outline-2
   ((default . (:foreground unspecified
                :inherit sol-strong-foreground))))
  (outline-3
   ((default . (:foreground unspecified
                :inherit sol-strong-foreground))))
  (outline-4
   ((default . (:foreground unspecified
                :inherit sol-strong-foreground))))
  (outline-5
   ((default . (:foreground unspecified
                :inherit sol-strong-foreground))))
  (outline-6
   ((default . (:foreground unspecified
                :inherit sol-strong-foreground))))
  (outline-7
   ((default . (:foreground unspecified
                :inherit sol-strong-foreground))))
  (outline-8
   ((default . (:foreground unspecified
                :inherit sol-strong-foreground)))))

(use-package dired-async
  :ensure nil
  :after (async config-theme) ;; part of this package
  :commands dired-async-mode
  :custom-face
  (dired-async-failures
   ((default . (:foreground ,sol-red))))
  (dired-async-message
   ((default . (:foreground unspecified
                :inherit sol-foreground))))
  (dired-async-mode-message
   ((default . (:foreground unspecified
                :inherit sol-foreground)))))

(use-package dired-auto-readme
  :disabled t
  :after dired
  :ensure (:host github :repo "amno1/dired-auto-readme")
  :commands (dired-auto-readme-mode)
  :general
  (dired-mode-map
   :states 'normal
   ")" '--toggle-dired-auto-readme-mode)
  :init
  (cl-defun --toggle-dired-auto-readme-mode ()
    (interactive)
    (if dired-auto-readme-mode
        (dired-auto-readme-mode -1)
      (dired-auto-readme-mode)))
  (add-hook 'dired-mode-hook 'dired-auto-readme-mode 99)
  :config
  (with-eval-after-load 'consult
    (define-advice consult-imenu (:around (func) toggle-dired-auto-readme-mode)
      (dired-auto-readme-mode -1)
      (condition-case err
          (funcall func)
        (t (dired-auto-readme-mode 1))
        (:success (dired-auto-readme-mode 1))))))

(use-package imenu
  :ensure nil
  :custom
  (imenu-auto-rescan t)
  :config
  (with-eval-after-load 'evil
    (add-hook 'imenu-after-jump-hook #'evil-set-jump)))

(use-package dired-imenu
  :after dired
  :init
  (add-hook 'dired-mode-hook
            #'(lambda ()
                (add-hook 'imenu-after-jump-hook #'dired-find-file nil t))))

(use-package eldoc
  :ensure nil
  :blackout t
  :custom
  (eldoc-echo-area-use-multiline-p 0.1)
  (eldoc-echo-area-prefer-doc-buffer nil)
  :config
  (global-eldoc-mode))

(use-package eglot-signature-eldoc-talkative
  :disabled t
  :ensure (:host codeberg :repo "mekeor/eglot-signature-eldoc-talkative")
  :after (eglot eldoc)
  :config
  (advice-add #'eglot-signature-eldoc-function
              :override #'eglot-signature-eldoc-talkative))

(use-package flymake
  :ensure nil
  :after config-theme
  :init
  (cl-defun --maybe-flymake-mode ()
    (if (buffer-file-name)
        (flymake-mode 1)
      (flymake-mode -1)))
  :hook
  (prog-mode-hook . --maybe-flymake-mode)
  :custom-face
  (flymake-error ((default . (:foreground unspecified
                              :box nil
                              :underline t
                              :inherit sol-error))))
  (flymake-error-echo ((default . (:inherit flymake-error))))
  (flymake-error-echo-at-eol ((default . (:inherit (flymake-error
                                                    sol-superlight-box)))))
  (flymake-note ((default . (:underline t
                             :inherit sol-note))))
  (flymake-note-echo ((default . (:inherit flymake-info))))
  (flymake-note-echo-at-eol ((default . (:inherit flymake-note))))
  (flymake-warning ((default . (:foreground unspecified
                                :underline t
                                :inherit sol-warning))))
  (flymake-warning-echo ((default . (:inherit flymake-warning))))
  (flymake-warning-echo-at-eol ((default . (:inherit flymake-warning))))
  :custom
  (flymake-error-bitmap nil)
  (flymake-warning-bitmap nil)
  (flymake-note-bitmap nil)
  (flymake-fringe-indicator-position nil)
  (flymake-indicator-type nil)
  (flymake-margin-indicator-position nil)
  (flymake-margin-indicators-string '((error "")
                                      (warning "")
                                      (note "")))
  (flymake-show-diagnostics-at-end-of-line t)
  :init
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "feb" #'flymake-show-buffer-diagnostics)
    (evil-ex-define-cmd "leb" "feb")
    (with-eval-after-load 'consult
      (evil-ex-define-cmd "fe" #'consult-flymake)
      (evil-ex-define-cmd "le" "fe")))
  :config
  (push '(before-string . nil) (get :note 'flymake-overlay-control))
  (push '(before-string . nil) (get :warning 'flymake-overlay-control))
  (push '(before-string . nil) (get :error 'flymake-overlay-control)))
  ;; :init
  ;; ;; solve the overlay problem
  ;; (setq flymake-overlay-control
  ;;       '(:error (before-string . nil)
  ;;         :warning (before-string . nil)
  ;;         :note (before-string . nil))))

(use-package eglot
  :ensure nil
  :after (config-theme evil flymake)
  :custom
  (eglot-prefer-plaintext t)
  (eglot-extend-to-xref t)
  :custom-face
  (eglot-highlight-symbol-face
   ((default . (:weight unspecified
                :inherit sol-superlight-background))))
  (eglot-inlay-hint-face
   ((default . (:height unspecified
                :inherit sol-light-foreground))
    (((supports (:italic)) (supports (:weight))) .
     (:italic t :weight semi-light))))
  :hook
  ((eglot-managed-mode-hook . eglot-inlay-hints-mode))
  :general
  (eglot-mode-map
   :states 'normal
    "g r" nil  ;; unbind `xref-find-references'
    "g 5" nil  ;; unbind `xref-find-definitions-other-frame'
    "g D" nil) ;; unbind `xref-find-definitions-other-window'
  :init
  (evil-ex-define-cmd "lmm"
                      #'(lambda ()
                          ;; Sometimes as consult loads flymake gets a bit trigger-happy linting.
                          ;; This resets the overlay.
                          (interactive)
                          (flymake-mode -1)
                          (flymake-mode 1)))
  (evil-ex-define-cmd "la" #'eglot-code-actions)
  (evil-ex-define-cmd "fla" "la")
  (evil-ex-define-cmd "lr" #'eglot-rename)
  (evil-ex-define-cmd "flr" "lr")
  ;; (evil-ex-define-cmd "lw" #'eglot-code-action-rewrite)
  ;; (evil-ex-define-cmd "lx" #'eglot-code-action-extract)
  ;; (evil-ex-define-cmd "li" #'eglot-code-action-inline)
  ;; (evil-ex-define-cmd "lc" #'eglot-code-action-quickfix)
  (evil-ex-define-cmd "lm" #'eglot-inlay-hints-mode)
  (evil-ex-define-cmd "lo" #'eglot-code-action-organize-imports))

(use-package consult-eglot
  :after (eglot consult)
  :commands (consult-eglot-symbols)
  :init
  (with-eval-after-load 'config-evil
    (evil-set-command-property #'consult-eglot-symbols :jump t)
    (--evil-define-splits "ll" #'consult-eglot-symbols)
    (--evil-define-splits "fl" "ll")
    (--evil-define-splits "fll" "ll")))

(use-package consult-eglot-embark
  :after (embark consult-eglot)
  :config
  (consult-eglot-embark-mode))

(use-package dired-git-info
  :disabled t
  :ensure (:host github :repo "clemera/dired-git-info")
  :after (dired config-theme)
  :hook
  (dired-after-readin-hook . dired-git-info-auto-enable)
  :custom-face
  (dgi-commit-message-face
   ((default . (:inherit sol-foreground)))))

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items nil)
  :config
  (recentf-mode)
  (with-eval-after-load 'consult
    (evil-ex-define-cmd "recent" 'consult-recent-file)))

(use-package autoinsert
  :ensure nil
  :config
  (auto-insert-mode))

(use-package bookmark
  :ensure nil
  :after config-theme
  :custom
  (bookmark-fringe-mark nil)
  (bookmark-save-flag 1 "Write to bookmark file immediately")
  :custom-face
  (bookmark-face
   ((default . (:inherit sol-subtle)))))
  ;; :config
  ;; (cl-defun config-define-bookmark (name path &optional overwrite annotation)
  ;;   "Programmatically creates and stores bookmarks into the bookmark file. We do
  ;;  this here because as of 2019-04-01T16:13:14+0800 we have no idea if there is an
  ;;  existing interface to do this. If one is found this will be marked obsolete and
  ;;  we'll move to that instead.

  ;;  The bookmark list format is found at `bookmark-alist'.

  ;;  NAME - Name of the bookmark. PATH - filepath of the bookmark. OVERWRITE - if
  ;;  true, overwrite an existing bookmark of the same name if one currently exists.
  ;;  ANNOTATION - Optional annotation of the bookmark.

  ;;  If PATH does not point to anywhere valid, this function is a no-op and no
  ;;  bookmark will be created."
  ;;   (require 'bookmark)
  ;;   (when (file-exists-p path)
  ;;     (let* ((annot (if annotation annotation ""))
  ;;            (alist `((filename . ,path)
  ;;                     (front-context-string . "")
  ;;                     (rear-context-string . "")
  ;;                     (position . 0)
  ;;                     (annotation . ,annot))))
  ;;       (bookmark-store name alist overwrite))))
  ;; (config-define-bookmark "init" user-init-file)
  ;; (config-define-bookmark "local" user-local-file))

(use-package diff
  :ensure nil
  :after config-theme
  :custom-face
  (diff-header
   ((default . (:inherit sol-foreground))))
  (diff-context
   ((default . (:inherit (sol-foreground
                          sol-superlight-background)))))
  (diff-added
   ((default . (:foreground ,sol-green
                :inherit (sol-foreground
                          sol-superlight-background)))))
  (diff-removed
   ((default . (:foreground ,sol-red
                :inherit (sol-foreground
                          sol-superlight-background)))))
  (diff-refined-changed
   ((default . (:foreground unspecified
                :inherit diff-changed))))
  (diff-refined-removed
   ((default . (:background ,sol-red
                :strike-through nil
                :inherit sol-foreground-i))))
  (diff-refined-added
   ((default . (:bold nil
                :background ,sol-green
                :inherit sol-foreground-i))))
  (diff-changed-unspecified
   ((default . (:background unspecified
                :inherit (diff-changed sol-superlight-background)))))
  (diff-error
   ((default . (:background unspecified
                :foreground ,sol-red))))
  (diff-indicator-added
   ((default . (:foreground unspecified))))
  (diff-indicator-changed
   ((default . (:foreground unspecified))))
  (diff-indicator-removed
   ((default . (:foreground unspecified)))))

(use-package whitespace
  :ensure nil
  :commands whitespace-mode
  :after config-theme
  :custom-face
  (whitespace-line
   ((default . (:foreground ,sol-violet))))
  (whitespace-newline
   ((default . (:foreground unspecified
                :inherit sol-light-foreground))))
  (whitespace-space
   ((default . (:foreground unspecified
                :background unspecified
                :inherit (sol-light-foreground))))))

(use-package ediff
  :ensure nil
  :commands (ediff ediff3)
  :after config-theme
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :custom-face
  ;; A
  (ediff-current-diff-A
   ((default . (:background unspecified
                :foreground ,sol-red
                :inherit sol-superlight-background))))
  (ediff-fine-diff-A
   ((default . (:background unspecified
                :inherit ediff-current-diff-A
                :bold t))))
  (ediff-even-diff-A
   ((default . (:background unspecified
                :foreground ,sol-red))))
  (ediff-odd-diff-A
   ((default . (:background unspecified
                :foreground ,sol-red))))
  ;; Ancestor
  (ediff-current-diff-Ancestor
   ((default . (:background unspecified
                :foreground ,sol-blue
                :inherit sol-superlight-background))))
  (ediff-fine-diff-Ancestor
   ((default . (:background unspecified
                :inherit ediff-current-diff-Ancestor
                :bold t))))
  (ediff-even-diff-Ancestor
   ((default . (:background unspecified
                :foreground ,sol-blue))))
  (ediff-odd-diff-Ancestor
   ((default . (:background unspecified
                :foreground ,sol-blue))))

  ;; B
  (ediff-current-diff-B
   ((default . (:background unspecified
                :foreground ,sol-green
                :inherit sol-superlight-background))))
  (ediff-fine-diff-B
   ((default . (:background unspecified
                :inherit ediff-current-diff-B
                 :bold t))))
  (ediff-even-diff-B
   ((default . (:background unspecified
                :foreground ,sol-green))))
  (ediff-odd-diff-B
   ((default . (:background unspecified
                :foreground ,sol-green))))

   ;; C
  (ediff-current-diff-C
   ((default . (:background unspecified
                :foreground ,sol-yellow
                :inherit sol-superlight-background))))
  (ediff-fine-diff-C
   ((default . (:background unspecified
                :inherit ediff-current-diff-C
                :bold t))))
  (ediff-even-diff-C
   ((default . (:background unspecified
                :foreground ,sol-yellow))))
  (ediff-odd-diff-C
   ((default . (:background unspecified
                :foreground ,sol-yellow)))))


(use-package calendar
  :ensure nil
  :commands calendar
  :custom
  (calendar-week-start-day 1 "monday"))

(use-package time
  :ensure nil
  :custom
  (display-time-24hr-format t))

(use-package tab-bar
  :ensure nil
  :after (general evil config-theme)
  :custom
  (tab-bar-close-last-tab-choice 'delete-frame)
  (tab-bar-new-tab-choice t)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-close-tab-select 'left)
  (tab-bar-new-button nil)
  (tab-bar-new-tab-to 'right)
  (tab-bar-tab-name-function #'tab-bar-tab-name-truncated)
  :custom-face
  (tab-bar ((default . (:foreground unspecified
                        :background unspecified
                        :inverse-video nil
                        :underline nil
                        :inherit sol-superlight-foreground))))
  (tab-bar-tab ((default . (:foreground unspecified
                            :background unspecified
                            :underline unspecified
                            :bold nil
                            :box nil
                            :inherit (sol-superlight-background
                                      sol-foreground)))))
  (tab-bar-tab-inactive ((default . (:foreground unspecified
                                     :background unspecified
                                     :bold nil
                                     :inverse-video nil
                                     :inherit sol-light-foreground))))
  (tab-line ((default . (:inverse-video nil
                         :foreground unspecified
                         :background unspecified
                         :inherit sol-light-foreground))))
  :general
  (:keymaps 'evil-window-map
   ;; single window in tab gets moved into frame
   "g f" #'(lambda ()
             (interactive)
             (unless (= 1 (length (window-list)))
               (tab-window-detach))
             (tab-detach))
   "g w" "g f"
   ;; entire tab gets moved into frame
   "g F" 'tab-detach
   "g W" "g F")
  ;; (:states 'normal
  ;;  "] t" #'tab-bar-switch-to-next-tab
  ;;  "[ t" #'tab-bar-switch-to-prev-tab)
  :init
  (evil-ex-define-cmd "gt" 'tab-bar-switch-to-next-tab)
  (evil-ex-define-cmd "gT" 'tab-bar-switch-to-prev-tab)
  (evil-define-command my-tab-bar-tab-edit (file)
    (interactive "<f>")
    (let ((tab-bar-new-tab-choice (if file file "*scratch*")))
      (tab-bar-new-tab)))
  (evil-ex-define-cmd "tabn[ew]" 'my-tab-bar-tab-edit)
  (evil-ex-define-cmd "tabe[dit]" 'tab-bar-new-tab)
  (evil-ex-define-cmd "tabc[lose]" 'tab-bar-close-tab)
  (evil-ex-define-cmd "tabo[ther]" 'tab-bar-close-other-tabs)
  (evil-define-command --tab-bar-rename-tab (name)
    (interactive "<a>")
    (tab-bar-rename-tab name))
  (evil-ex-define-cmd "tabr[ename]" '--tab-bar-rename-tab)
  (evil-ex-define-cmd "tabs" 'tab-bar-select-tab-by-name)
  (evil-ex-define-cmd "tabm[ove]+" 'tab-bar-move-tab)
  (evil-ex-define-cmd "tabm[ove]-" 'tab-bar-move-tab-right)
  (evil-ex-define-cmd "tabd[etach]" 'tab-detach)
  :config
  (tab-bar-mode)
  ;; (cl-defun --tab-bar-tab-name-fn ()
  ;;   (require 'project)
  ;;   (let ((buffer-name (-> (minibuffer-selected-window)
  ;;                          (window-buffer)
  ;;                          (buffer-name))))
  ;;     (if-let* ((project-info (project-current)))
  ;;         (format "%s<%s>" buffer-name (project-root project-info))
  ;;       (format "%s" buffer-name))))
  ;; (customize-set-value 'tab-bar-tab-name-function #'--tab-bar-tab-name-fn)

  (define-advice delete-frame (:around (oldfun &rest _old_args)
                                       --tab-bar-delete-tab-or-emacs)
    (interactive)
    (let* ((tabs (cl-find-if (lambda (elem) (eq 'tabs (car elem)))
                          (frame-parameters)))
           (num-tabs (length (cdr tabs))))
      (if (eq num-tabs 1)
          (call-interactively oldfun)
        (tab-bar-close-tab)))))

  ;; (with-eval-after-load 'transient
  ;;   (transient-define-prefix --tab-bar ()
  ;;     [["Search"
  ;;       ("t" "Select tab by name" tab-bar-select-tab-by-name)]
  ;;      ["Move"
  ;;       (">>" "Move right" tab-bar-move-tab :transient t)
  ;;       ("<<" "Move left" tab-bar-move-tab-backward :transient t)]
  ;;      ["Manipulate"
  ;;       ("r" "Rename" tab-bar-rename-tab)
  ;;       ("c" "Close" tab-bar-close-tab)
  ;;       ("g" "Group" tab-bar-change-tab-group)]])
  ;;   (evil-ex-define-cmd "tt" '--tab-bar)))

(use-package grep
  :ensure nil
  :config
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "lgr" #'lgrep)
    (with-eval-after-load 'consult
      (evil-set-command-property #'consult-grep :jump t)
      (evil-ex-define-cmd "gr" #'consult-grep))))

(use-package autorevert
  :ensure nil
  :custom
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode))

;; (use-package apropos
;;   :ensure nil
;;   :custom
;;   (apropos-do-all t)
;;   :init
;;   (with-eval-after-load 'evil
;;     (evil-define-command my-apropos (pattern)
;;       (interactive "<a>")
;;       (if pattern
;;           (apropos pattern)
;;         (info)))
;;     (evil-ex-define-cmd "h[elp]" 'my-apropos)))

(use-package ibuffer
  :ensure nil
  :hook
  (ibuffer-mode-hook . hl-line-mode))

(use-package compile
  :ensure nil
  :after config-theme
  :custom
  (compilation-auto-jump-to-first-error nil)
  (compilation-ask-about-save nil)
  (compilation-always-kill t)
  (compile-command "" "Set the default to nothing")
  :custom-face
  (compilation-mode-line-run
   ((default . (:inherit sol-foreground))))
  (compilation-info
   ((default . (:foreground unspecified
                :bold nil
                :inherit sol-info))))
  (compilation-warning
   ((default . (:foreground unspecified
                :bold nil
                :inherit sol-warning))))
  (compilation-error
   ((default . (:foreground unspecified
                :inherit sol-error))))
  (compilation-line-number
   ((default . (:foreground unspecified
                :background unspecified
                :inherit sol-light-foreground))))
  (compilation-column-number
   ((default . (:inherit compilation-line-number))))
  (compilation-mode-line-fail
   ((default . (:foreground unspecified
                :inherit compilation-error))))
  (compilation-mode-line-exit
   ((default . (:foreground unspecified
                :inherit sol-foreground))))
  :config
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'compile-history)
    (add-to-list 'savehist-additional-variables 'compile-command))
  (with-eval-after-load 'consult
    (general-define-key
     :keymaps 'compilation-mode-map
     :states 'normal
     "f f" #'consult-compile-error))
  ;; (defun my-colorize-completion-buffer ()
  ;;   (require 'ansi-color)
  ;;   (let ((inhibit-read-only t))
  ;;     (ansi-color-apply-on-region compilation-filter-start (point))))
  :general
  (compilation-mode-map
   :states 'normal
   "]]" 'compilation-next-error
   "[[" 'compilation-previous-error)
  :hook
  ;; (compilation-filter-hook . my-colorize-completion-buffer)
  (compilation-mode-hook . hl-line-mode)
  (compilation-mode-hook . visual-line-mode))

(use-package abbrev
  :ensure nil
  :blackout t
  :custom
  (abbrev-file-name (locate-user-emacs-file "abbrevs.el"))
  (save-abbrevs 'silently)
  (only-global-abbrevs t)
  :general
  (edit-abbrevs-mode-map
   [remap evil-save] 'abbrev-edit-save-buffer)
  (:states '(normal visual)
   :prefix my-default-evil-leader-key
   "a a" 'inverse-add-global-abbrev)
  :init
  ;; This makes it active globally
  (setq-default abbrev-mode t)
  (with-eval-after-load 'evil
   (evil-define-command my-inverse-add-global-abbrev (beg end _type)
     "Generates an abbrev for the given visual selection."
     (interactive "<v>")
     (require 'abbrev)
     ;; nil if there is no visual region; the number of words in the
     ;; region otherwise
     ;; (add-global-abbrev
     ;;  (if (and beg end)
     ;;      (-> (buffer-substring-no-properties beg end)
     ;;          (split-string)
     ;;          (length))
     ;;    1))))
     ;; (evil-define-command ex-abbreviation (arg)
     ;;   "Attempts to replicate the :abbreviate function in vim.
     ;; :ab - Shows you the abbreviation tables
     ;; :ab A B [C D E] - Expands \"A\" to \"B C D E\". At least 2 arguments
     ;; must be given, otherwise it's a no-op."
     ;;   (interactive "<a>")
     ;;   ;; TODO: figure out what happens if we want the expansion to have
     ;;   ;; variable space length. We currently don't care, because odds are
     ;;   ;; we want our expansion to look like good english anyway.
     ;;   (if (not arg)
     ;;       (edit-abbrevs)
     ;;     (let* ((arguments (split-string arg))
     ;;            (size (safe-length arguments)))
     ;;       (cond
     ;;        ((= size 1) nil) ;; no op, as far as I can tell
     ;;        (t (let ((expansion (mapconcat 'identity (cdr arguments) " ")))
     ;;             (define-global-abbrev (car arguments) expansion)))))))
     ;; (evil-ex-define-cmd "ab[breviate]" 'ex-abbreviation))
     ;; (if (and beg end)
     ;;     (let ((current-prefix-arg 0))
     ;;       (call-interactively 'add-global-abbrev))
     ;;   (call-interactively 'inverse-add-global-abbrev))
     (let* ((name (if (region-active-p)
                      (buffer-substring-no-properties beg end)
                    (thing-at-point 'word)))
            (exp (read-string (format "abbrev for \"%s\": " name))))
       (define-abbrev global-abbrev-table (downcase name) exp)))))

(use-package comint
  :ensure nil
  :custom
  ;; Make cursor always move to end when entering insert mode in
  ;; comint modes
  (comint-scroll-to-bottom-on-input t)
  (comint-prompt-read-only t)
  :general
  (comint-mode-map
   :states '(normal insert)
   "C-l" #'comint-clear-buffer))

;; Make shell open in same window
;; - Related Spacemacs Issue :: https://github.com/syl20bnr/spacemacs/issues/6820
;; - Make shell mode update working directory :: https://emacs.stackexchange.com/questions/5589/automatically-update-default-directory-when-pwd-changes-in-shell-mode-and-term-m
(use-package shell
  :ensure nil
  :commands shell
  :custom
  (comint-scroll-to-bottom-on-input t)
  (comint-prompt-read-only t)
  :config
  (with-eval-after-load 'evil
    (general-define-key
     :keymaps 'shell-command-mode-map
     :states 'normal
      "q" #'kill-current-buffer)
    (add-to-list 'evil-normal-state-modes 'shell-command-mode))
  :init
  (defun my-buffer-specific-shell ()
    (interactive)
    (let ((name (format "*shell<%s>*" (buffer-name))))
      (shell name)))
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "sh[ell]" #'(lambda () (interactive)
                                      (my-buffer-specific-shell)))
    (evil-ex-define-cmd "Sshell" #'(lambda () (interactive)
                                     (evil-window-split)
                                     (my-buffer-specific-shell)))
    (evil-ex-define-cmd "Vshell" #'(lambda () (interactive)
                                     (evil-window-vsplit)
                                     (my-buffer-specific-shell))))
  (add-to-list 'display-buffer-alist '("\\*shell\\*" . (display-buffer-same-window . nil)))
  :hook
  (shell-mode-hook . shell-dirtrack-mode)
  :config
  (with-eval-after-load 'org
   (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))))

(use-package eshell
  :ensure nil
  :commands eshell
  :after config-theme
  :custom-face
  (eshell-prompt
   ((default . (:foreground ,sol-green)))))

(use-package isearch
  :ensure nil
  :after config-theme
  :custom-face
  (isearch
   ((default . (:background unspecified
                :foreground ,sol-green
                :inherit sol-superlight-background))))
  (lazy-highlight
   ((default . (:distant-foreground unspecified
                :background unspecified
                :inherit match)))))

(use-package simple
  :ensure nil
  :after config-theme
  :custom-face
  (separator-line ((default . (:inherit 'sol-superlight-background))))
  :config
  ;; not a thing in older emacs versions
  (when (facep 'blink-matching-paren-offscreen)
    (custom-set-faces
     `(blink-matching-paren-offscreen
       ((default . (:inherit sol-strong-background))
        (((supports (:bold))) . (:bold t)))))))

(use-package so-long
  :ensure nil
  :custom
  (so-long-threshold 5000)
  :config
  (global-so-long-mode))

(use-package calc
  :ensure nil
  :commands calc
  :general
  (calc-edit-mode-map
   "C-c C-k" '(lambda () (interactive) (kill-buffer (current-buffer)))))

(use-package gud
  :ensure nil
  :after (evil transient)
  :mode ("\\.gdb\\'" . gdb-script-mode)
  :custom
  (gud-highlight-current-line t)
  :init
  (cl-defun --invoke-gdb ()
    (interactive)
    (require 'project)
    (if-let* ((project (project-current t))
              (default-directory (project-root project)))
        (call-interactively 'gdb)
      (gdb)))
  (evil-ex-define-cmd "gdb"
                      #'(lambda () (interactive)
                          (require 'gud)
                          ;; if `gud-break' is not defined, then we haven't
                          ;; called `gdb' at least once. For the sake of the
                          ;; transient, fast-forward to the invocation.
                          (if (not (fboundp 'gud-break))
                              (--invoke-gdb)
                            (--gdb))))
  (evil-ex-define-cmd "gud" "gdb")
  :config
  (transient-define-prefix --gdb ()
   ["`gdb-mi' command dispatcher.\n"
    ["Debugger"
     ("RET" "Start debugging" --invoke-gdb)
     ("g r" "Refresh" gud-refresh)]

    ["Point-based commands"
     ("b" "Set breakpoint" gud-break)
     ("t" "Set temporary breakpoint" gud-tbreak)
     ("d" "Remove breakpoint" gud-remove)
     ("p" "Evaluate expression" gud-remove)
     ("u" "Execute to this line" gud-until)]

    ["Repl manipulation"
     ("f u" "Up frame" gud-up :transient t)
     ("f d" "Down frame" gud-down :transient t)
     ("n" "Step skipping functions" gud-next :transient t)
     ("s" "Step" gud-step :transient t)
     ("c" "Continue" gud-cont)]])

  (cl-defun --gdb-point-to-linespec ()
    "Generate a linespec compatible with gdb's `break' <FILENAME>:<LINE>"
    (interactive)
    (let* ((filename (file-name-nondirectory (buffer-file-name)))
           (line-num (line-number-at-pos (point) t))
           (linespec (format "%s:%s" filename line-num)))
      (kill-new linespec)
      (message "%s" linespec)))
  (cl-defun --gdb-linespec-to-point ()
    "From a linespec, find the originating file.
It's quite stupid at the moment, and assumes the line starts with `break'"
    (interactive)
    (require 'project)
    (let* ((line (thing-at-point 'line :no-properties))
           (_ (string-match "break \\(.*\\):\\([0-9]+\\)" line))
           (filename (match-string 1 line))
           (num (string-to-number (match-string 2 line)))
           (matches (seq-filter (lambda (f)
                                  (string-suffix-p filename f))
                                (project-files (project-current))))
           (match (if (length= matches 1)
                      (car matches)
                    (--completing-read "File: " matches :require-match t))))
      (find-file match)
      (goto-line num))))

(use-package smerge-mode
  :ensure nil
  :after (transient evil config-theme)
  :commands smerge-mode
  :custom-face
  (smerge-markers
   ((default . (:foreground ,sol-cyan
                :background unspecified
                :inherit sol-light-foreground))))
  (smerge-base
   ((default . (:background unspecified
                :foreground ,sol-blue))))
  (smerge-lower
   ((default . (:background unspecified
                :foreground ,sol-yellow))))
  (smerge-upper
   ((default . (:background unspecified
                :foreground ,sol-violet))))
  (smerge-refined-added
   ((default . (:background unspecified
                :foreground ,sol-green))))
  (smerge-refined-removed
   ((default . (:background unspecified
                :foreground ,sol-red))))
  :init
  (transient-define-prefix --smerge ()
    ["Smerge mode command dispatcher."
     ["Navigation"
      ("k" "Previous hunk" (lambda ()
                             (interactive)
                             (smerge-prev)
                             (recenter)) :transient t)
      ("j" "Next hunk" (lambda () (interactive)
                         (smerge-next)
                         (recenter)) :transient t)]
     ["Selection"
      ("u" "Keep upper hunk" smerge-keep-upper :transient t)
      ("l" "Keep lower hunk" smerge-keep-lower :transient t)
      ("a" "Keep all hunks" smerge-keep-all :transient t)
      ("b" "Keep base hunk" smerge-keep-base :transient t)
      ("c" "Keep hunk at point" smerge-keep-current :transient t)
      ("s" "Resolve ('intelligently')" smerge-resolve :transient t)]
     ["Display"
      ("r" "Cycle Refine view" smerge-refine :transient t)
      ("d u" "Diff base/upper" smerge-diff-base-upper :transient t)
      ("d l" "Diff base/lower" smerge-diff-base-lower :transient t)
      ("d c" "Diff upper/lower" smerge-diff-upper-lower :transient t)]
     ["???? What are these commands"
      ("m" "Combine with Next" smerge-combine-with-next)]])
  (evil-ex-define-cmd "smerge" #'--smerge))

(use-package term
  :ensure nil
  :after config-theme
  :commands term
  :custom-face
  (term-color-red
   ((default . (:foreground ,sol-red
                :background ,sol-red))))
  (term-color-green
   ((default . (:foreground ,sol-green
                :background ,sol-green))))
  (term-color-yellow
   ((default . (:foreground ,sol-yellow
                :background ,sol-yellow))))
  (term-color-green
   ((default . (:foreground ,sol-green
                :background ,sol-green))))
  (term-color-blue
   ((default . (:foreground ,sol-blue
                :background ,sol-blue))))
  (term-color-magenta
   ((default . (:foreground ,sol-magenta
                :background ,sol-magenta))))
  (term-color-cyan
   ((default . (:foreground ,sol-cyan
                :background ,sol-cyan))))
  (term-color-white
   ((default . (:foreground ,sol-base2
                :background ,sol-base2)))))

;; this is done primarily for vterm, so if we're not using vterm these
;; values might be wrong
(use-package ansi-color
  :ensure nil
  :after config-theme
  :custom-face
  (ansi-color-bright-black
   ((default . (:foreground ,sol-base03
                :background ,sol-base03))))
  (ansi-color-black
   ((default . (:foreground ,sol-base02
                :background ,sol-base02))))
  (ansi-color-bright-blue
   ((default . (:foreground ,sol-base0
                :background ,sol-base0))))
  (ansi-color-blue
   ((default . (:foreground ,sol-blue
                :background ,sol-blue))))
  (ansi-color-bright-cyan
   ((default . (:foreground ,sol-base1
                :background ,sol-base1))))
  (ansi-color-cyan
   ((default . (:foreground ,sol-cyan
                :background ,sol-cyan))))
  (ansi-color-bright-green
   ((default . (:foreground ,sol-base01
                :background ,sol-base01))))
  (ansi-color-green
   ((default . (:foreground ,sol-green
                :background ,sol-green))))
  (ansi-color-bright-magenta
   ((default . (:foreground ,sol-violet
                :background ,sol-violet))))
  (ansi-color-magenta
   ((default . (:foreground ,sol-magenta
                :background ,sol-magenta))))
  (ansi-color-bright-red
   ((default . (:foreground ,sol-orange
                :background ,sol-orange))))
  (ansi-color-red
   ((default . (:foreground ,sol-red
                :background ,sol-red))))
  (ansi-color-bright-white
   ((default . (:foreground ,sol-base3
                :background ,sol-base3))))
  (ansi-color-bright-yellow
   ((default . (:foreground ,sol-base00
                :background ,sol-base00))))
  (ansi-color-yellow
   ((default . (:foreground ,sol-yellow
                :background ,sol-yellow)))))

(use-package xref
  :ensure nil
  :config
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package pulse
  :ensure nil
  :after config-theme
  :custom-face
  (pulse-highlight-start-face
   ((default . (:background unspecified
                :inherit sol-superlight-background)))))

(use-package info
  :ensure nil
  :after (config-theme general evil link-hint)
  :general
  (Info-mode-map
   :states 'normal
   "f" 'link-hint-open-link
   "]]" 'Info-next
   "[[" 'Info-prev)
  :config
  (with-eval-after-load 'consult
    (evil-ex-define-cmd "h[elp]" #'consult-info)
    (with-eval-after-load 'config-evil-helpers
      (--evil-define-splits "info" #'info)))
  :custom-face
  (info-node
   ((default . (:inherit sol-foreground)))))

(use-package shortdoc
  :ensure nil
  :after evil
  :commands shortdoc
  :init
  (evil-ex-define-cmd "cheatsheet" 'shortdoc)
  (evil-ex-define-cmd "cs" "cheatsheet")
  (evil-ex-define-cmd "hh" "cheatsheet"))

(use-package diff-mode
  :ensure nil
  :hook
  (diff-mode-hook . font-lock-mode))

(provide 'config-emacs)

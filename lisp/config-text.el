;;; config-text.el --- Text related packages

(use-package smartparens
  :ensure (:host github :repo "Fuco1/smartparens")
  :demand t
  :blackout t
  :hook
  ;; TODO: make this not just hooked on prog-mode
  (prog-mode-hook . (lambda () (interactive)
                      (require 'smartparens-config) ;; load some default configurations
                      (require 'smartparens)
                      (smartparens-mode)))
  :custom-face
  (sp-pair-overlay-face ((t (:inherit default :underline nil))))
  :general
  (:states 'normal
   :prefix my-default-evil-leader-key
   "," 'smartparens-mode)
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

(use-package aggressive-fill-paragraph
  :ensure (:host github :repo "davidshepherd7/aggressive-fill-paragraph-mode")
  :commands (aggressive-fill-paragraph-mode)
  :general
  (:states 'normal
   :prefix my-default-evil-leader-key
   "g w" 'aggressive-fill-paragraph-mode)
  :hook ((org-mode-hook . aggressive-fill-paragraph-mode)))

(use-package visual-fill-column
  :after general)

(use-package aggressive-indent
  :ensure (:host github :repo "malabarba/aggressive-indent-mode")
  :commands (aggressive-indent-mode)
  :custom
  (aggressive-indent-comments-too t)
  :general
  (:states 'normal
   :prefix my-default-evil-leader-key
   "=" 'aggressive-indent-mode))

(use-package yasnippet
  :disabled t
  :ensure (:host github :repo "joaotavora/yasnippet")
  :commands (yas-minor-mode
             yas-expand-snippet)
  :hook
  ((prog-mode-hook . yas-minor-mode)
   (org-mode-hook . yas-minor-mode))
  :general
  (yas-keymap
   "C-j" 'yas-next-field-or-maybe-expand
   "C-k" 'yas-prev-field)
  (:states '(normal visual)
   :prefix my-default-evil-leader-key
   "s s" 'yas-insert-snippet
   "s n" 'yas-new-snippet
   "s f" 'yas-visit-snippet-file)
  (snippet-mode-map
   [remap evil-save-and-close]          'yas-load-snippet-buffer-and-close
   [remap evil-save-modified-and-close] 'yas-load-snippet-buffer-and-close
   [remap evil-quit]                    'kill-this-buffer)
  :custom
  (yas-snippet-dirs (list (file-name-as-directory
                           (locate-user-emacs-file "snippets"))))
  (yas-indent-line 'auto)
  (yas-also-auto-indent-first-line t)
  :config
  (with-eval-after-load 'blackout
    (blackout--handle-minor-mode 'yas-minor-mode))
  (defun yas-with-comment (str)
    ;; TODO: note that this is a hack; the proper way should be
    ;; something as written in the comment box. That said, the
    ;; "proper" way is also not working.

    ;; (with-temp-buffer
    ;;   (format "%s" str)
    ;; this might explain why this function seems to bug out sometimes.
    ;;   (comment-normalize-vars)
    ;;   (comment-region (point-min) (point-max))
    ;;   (buffer-string)))
    (let ((comment-start (cond ((eq major-mode 'emacs-lisp-mode) ";; ")
                               ((eq major-mode 'terraform-mode) "# ")
                               (t comment-start))))
      ;; if we are already within a comment then skip comment char
      ;; insertion.
      (if (save-excursion (comment-beginning))
          (format "%s" str)
        (format "%s%s%s" comment-start str comment-end))))
  (yas-global-mode))

(use-package yasnippet-capf
  :disabled t
  :after (cape yasnippet)
  :custom
  (yasnippet-capf-lookup-by 'name)
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package auto-yasnippet
  :disabled t
  :ensure (:host github :repo "abo-abo/auto-yasnippet")
  :blackout t
  :after yasnippet
  :commands (aya-create
             aya-expand)
  :custom
  (aya-case-fold t "smartcasing"))

;; template syntax
;; https://github.com/minad/tempel?tab=readme-ov-file#template-syntax
(use-package tempel
  :ensure (:host github :repo "minad/tempel")
  :custom-face
  (tempel-field ((default . (:inherit sol-strong-foreground
                             :foreground unspecified
                             :background unspecified))))
  (tempel-form ((default . (:inherit sol-foreground
                            :foreground ,sol-orange
                            :background unspecified))))
  (tempel-default ((default . (:inherit sol-light-foreground
                               :foreground unspecified
                               :background unspecified))))
  :general
  (tempel-map
   "TAB"       #'--tempel-maybe-expand-or-jump
   "<backtab>" #'tempel-previous
   "M-SPC"     #'tempel-kill
   "M-l"       #'tempel-next
   "M-j"       #'tempel-next
   "M-h"       #'tempel-previous
   "M-k"       #'tempel-previous
   "M-q"       #'tempel-abort)
  :init
  ;; turn it into a list so we can define local configs
  (setq tempel-path (list (locate-user-emacs-file "templates.eld")))
   ;; Setup completion at point
  (cl-defun --tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (cl-defun --tempel-maybe-expand-or-jump ()
    (interactive)
    (if-let ((templates (tempel--templates))
             (bounds (tempel--prefix-bounds))
             (name (buffer-substring-no-properties
                    (car bounds) (cdr bounds)))
             (sym (intern-soft name))
             (template (assq sym templates)))
        (tempel-expand template)
      (tempel-next 1)))

  (cl-defun --tempel-maybe-comment ()
    (unless (save-excursion (comment-beginning))
      (cond ((derived-mode-p 'emacs-lisp-mode) ";; ")
            ((derived-mode-p 'terraform-mode) "# ")
            (t comment-start))))
  :hook
  (conf-mode-hook . --tempel-setup-capf)
  (prog-mode-hook . --tempel-setup-capf)
  (text-mode-hook . --tempel-setup-capf)
  (eglot-managed-mode-hook . --tempel-setup-capf))

;; For M1 machines, we have to clone
;; https://github.com/eraserhd/parinfer-rust.git, build the =.dylib=, and
;; rename the extension to =.so=:
;;
;; ```
;; cargo build --release --features emacs
;; cp target/release/libparinfer_rust.dylib ~/.emacs.d/parinfer-rust/parinfer-rust-darwin.so
;; ```
;;
;; Note that dired filters might/will hide files with the `.so' extension so
;; you're going to have to check that that isn't happening.
;;
;; parinfer-rust-mode requires track-changes 1.1
(use-package parinfer-rust-mode
  :after (track-changes flymake flycheck)
  :blackout t
  :general
  (:states 'motion
   "g p" 'parinfer-rust-toggle-paren-mode)
  :init
  (elpaca-wait)
  :custom
  (parinfer-rust-auto-download t)
  (parinfer-rust-dim-parens nil)
  :hook ((emacs-lisp-mode-hook . parinfer-rust-mode)
         (racket-mode-hook . parinfer-rust-mode)
         (clojure-mode-hook . parinfer-rust-mode)
         (hy-mode-hook . parinfer-rust-mode)))

(use-package prism
  :disabled t ;; see if I can live without this for a bit
  :ensure (:host github :repo "alphapapa/prism.el")
  :commands (prism-mode
             prism-whitespace-mode)
  :hook
  ((racket-mode-hook . prism-mode)
   (clojure-mode-hook . prism-mode)
   (json-mode-hook . prism-mode)
   (emacs-lisp-mode-hook . prism-mode)
   (hy-mode-hook . prism-mode))
  :preface
  (use-package anaphora
    :ensure (:host github :repo "rolandwalker/anaphora")))

(use-package breadcrumb
  :ensure (:host github :repo "joaotavora/breadcrumb")
  :config
  (breadcrumb-mode)
  (with-eval-after-load 'so-long
    (add-hook 'so-long-mode-hook #'(lambda ()
                                     (breadcrumb-local-mode -1)))))

(use-package wgrep
  :ensure (:host github :repo "mhayashi1120/Emacs-wgrep")
  :commands (wgrep-change-to-wgrep-mode)
  :custom (wgrep-auto-save-buffer t))

(use-package rg
  :if (executable-find "rg")
  :ensure (:host github :repo "dajva/rg.el")
  :demand t
  :after (evil general)
  :custom
  (rg-ignore-case 'smart)
  (rg-keymap-prefix "")
  (rg-default-alias-fallback "everything")
  (rg-buffer-name #'(lambda () (format "*rg<%s>*" (buffer-name))))
  :general
  (rg-mode-map
   :states '(motion normal)
   "gg" 'evil-goto-first-line)
  (rg-mode-map
   :states 'normal
    "M-j" "C-j"
    "M-k" "C-k")
  (grep-mode-map
   :states '(motion normal)
   "n" 'evil-ex-search-next
   "N" 'evil-ex-search-previous)
  (:states '(normal motion visual)
   "C-+" 'rg-menu)
  :init
  (evil-ex-define-cmd "rr" #'rg-menu)
  (evil-ex-define-cmd "rg" "rr")
  (with-eval-after-load 'consult
    (evil-ex-define-cmd "rr" #'consult-ripgrep)
    (evil-ex-define-cmd "rl"
                        ;; we can't do regions because ex-commands
                        ;; always widen to the line
                        #'(lambda () (interactive)
                            (require 'thingatpt)
                            (consult-ripgrep nil
                                             (thing-at-point 'symbol))))
    (evil-ex-define-cmd "lr" "rl"))
  (evil-ex-define-cmd "rf" #'--rg-search-file)
  (evil-ex-define-cmd "rd" #'--rg-search-dir)
  ;; (evil-ex-define-cmd "prg" 'rg-project)
  :config
  (rg-enable-menu)
  ;; (transient-remove-suffix 'rg-menu "d")
  (transient-remove-suffix 'rg-menu "c")
  ;; (transient-remove-suffix 'rg-menu "f")

  ;; Note that this is not a true 1-file search. It's all the files with the
  ;; same name within the directory
  ;; https://github.com/dajva/rg.el/issues/91
  (rg-define-search --rg-search-file
    :files (funcall #'(lambda () (file-name-nondirectory (buffer-file-name))))
    :dir current
    :query ask
    :menu ("Search" "f" "File"))
  (rg-define-search --rg-search-dir
    :files "everything"
    :dir current
    :query (funcall #'--thing-at-point-or-region-or-user-input)
    :menu ("Search" "d" "Directory"))
  (with-eval-after-load 'hl-todo
    (rg-define-search search-hl-todo-keywords
      "Uses the everything filter for project searches"
      :query (-> (mapcar 'car hl-todo-keyword-faces)
                 (string-join "|"))
      :format regexp
      :files "everything" ;; make this "all" maybe?
      :dir project
      :menu ("Custom" "hl" "`hl-todo' Keywords"))
    (evil-ex-define-cmd "hl-todo" 'search-hl-todo-keywords)))

(use-package origami
  :if (or (eq system-type 'windows-nt)
          (not (and (fboundp 'treesit-available-p)
                    (treesit-available-p))))
  :ensure (:host github :repo "gregsexton/origami.el")
  :after evil
  :defer 2
  :general
  (:states 'normal
   "zm" 'origami-close-all-nodes
   "zr" 'origami-open-all-nodes
   "zc" 'origami-close-node
   "zC" 'origami-close-node-recursively
   "zo" 'origami-open-node
   "zO" 'origami-open-node-recursively
   "za" 'origami-recursively-toggle-node
   "zj" 'origami-forward-fold
   "zk" #'(lambda () (interactive)
            (call-interactively 'origami-previous-fold)
            (call-interactively 'origami-next-fold))
   "[z" 'origami-previous-fold
   "]z" 'origami-next-fold)
  :hook
  (prog-mode-hook . origami-mode)
  :config
  (global-origami-mode))

(use-package tree-sitter
  :if (eq system-type 'windows-nt)
  :blackout t
  :commands (tree-sitter-hl-mode tree-sitter-mode)
  :hook ((tree-sitter-after-on-hook . tree-sitter-hl-mode))
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package treesit-auto
  :if (and (fboundp 'treesit-available-p)
           (treesit-available-p)
           (not (eq system-type 'windows-nt)))
  :custom
  (treesit-auto-install t)
  :config
  (global-treesit-auto-mode))

(use-package treesit-fold
  :if (and (fboundp 'treesit-available-p)
           (treesit-available-p)
           (not (eq system-type 'windows-nt)))
  :ensure (:host github :repo "emacs-tree-sitter/treesit-fold" :branch "master")
  :general
  (:states 'normal
   "za" #'treesit-fold-toggle
   "zc" #'treesit-fold-close
   "zC" #'treesit-fold-close-recursively
   "zo" #'treesit-fold-open
   "zO" #'treesit-fold-open-recursively
   "zm" #'treesit-fold-close-all
   "zr" #'treesit-fold-open-all)
  :config
  (global-treesit-fold-mode))

;; dice rolling and the like
(use-package decide)

;; Zebra patterns for buffer
(use-package stripe-buffer
  :commands stripe-buffer-mode)

(use-package dogears
  :demand t
  :after (evil)
  :commands (dogears-list dogears-remember dogears-go)
  :custom
  (dogears-idle 10)
  (dogears-position-delta 500)
  (dogears-ellipsis "...")
  :general
  (dogears-list-mode-map
   :states 'normal
   "RET" 'dogears-list-go
   "o" 'tabulated-list-sort
   "d d" 'dogears-list-delete
   "M-l" 'tabulated-list-next-column
   "M-h" 'tabulated-list-previous-column)
  :init
  (evil-ex-define-cmd "da" 'dogears-list)
  (evil-ex-define-cmd "dr" 'dogears-remember)
  (evil-ex-define-cmd "dd" #'(lambda () (interactive)
                               (let ((prefix-arg 1))
                                 (command-execute #'dogears-go))))
  (evil-ex-define-cmd "dn" 'dogears-forward)
  (evil-ex-define-cmd "dp" 'dogears-back)
  ;; note that ":di" is bound to "display", which is an alias for "registers"
  ;; which displays current register contents.
  :config
  (dogears-mode)
  (add-to-list 'dogears-hooks 'xref-after-jump-hook)
  (add-to-list 'dogears-hooks 'bookmark-after-jump-hook)
  (with-eval-after-load 'elpaca
    (add-to-list 'dogears-ignore-modes 'elpaca-log-mode))
  (with-eval-after-load 'git-commit
    (add-to-list 'dogears-ignore-modes 'git-commit-mode))
  (with-eval-after-load 'magit-status
    (add-to-list 'dogears-ignore-modes 'magit-status-mode)))

(use-package focus
  :commands focus-mode
  :after evil
  :init
  (evil-ex-define-cmd "fo[cus]" 'focus-mode)
  (evil-ex-define-cmd "fou" 'focus-unpin)
  (evil-ex-define-cmd "fop" 'focus-pin)
  (with-eval-after-load 'embark
    (with-eval-after-load 'general
      (general-define-key
       :keymaps 'embark-defun-map
        "f f" 'focus-mode
        "f u" 'focus-unpin
        "f p" 'focus-pin))))

(use-package lsp-focus
  :after (lsp focus)
  :hook
  (focus-mode-hook . #'lsp-focus-mode))

(use-package ace-window
  :after (evil general)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :general
  (evil-window-map
   "SPC" 'ace-window))

(provide 'config-text)

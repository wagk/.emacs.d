;;; config-text.el --- Text related packages

(use-package aggressive-fill-paragraph
  :ensure (:host github :repo "davidshepherd7/aggressive-fill-paragraph-mode")
  :commands (aggressive-fill-paragraph-mode)
  :general
  (:states 'normal
   :prefix my-default-evil-leader-key
   "g w" 'aggressive-fill-paragraph-mode)
  :hook ((org-mode-hook . aggressive-fill-paragraph-mode)
         (markdown-mode-hook . aggressive-fill-paragraph-mode)))

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
      (format "%s%s%s" comment-start str comment-end)))
  (yas-global-mode))

(use-package yasnippet-capf
  :after (:all cape yasnippet)
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package auto-yasnippet
  :ensure (:host github :repo "abo-abo/auto-yasnippet")
  :after yasnippet
  :commands (aya-create
             aya-expand)
  :custom
  (aya-case-fold t "smartcasing"))

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
(use-package parinfer-rust-mode
  :commands (parinfer-rust-mode)
  :general
  (:states 'motion
   "g p" 'parinfer-rust-toggle-paren-mode)
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

(use-package wgrep
  :ensure (:host github :repo "mhayashi1120/Emacs-wgrep")
  :commands (wgrep-change-to-wgrep-mode)
  :custom (wgrep-auto-save-buffer t))

(use-package rg
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
  (evil-ex-define-cmd "rg" #'rg-menu)
  (evil-ex-define-cmd "rr" #'rg-menu)
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
  :commands (tree-sitter-hl-mode tree-sitter-mode)
  :hook ((tree-sitter-after-on-hook . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package treesit-auto
  :disabled t ;; tree-sitter is more mature, I find
  :if (and (not (eq system-type 'windows-nt))
           (fboundp 'treesit-available-p)
           (treesit-available-p))
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

;; dice rolling and the like
(use-package decide
  :ensure t
  :config
  (decide-mode))

;; Zebra patterns for buffer
(use-package stripe-buffer
  :commands stripe-buffer-mode)

(use-package dogears
  :disabled t
  :demand t
  :after (evil)
  :commands (dogears-list dogears-remember dogears-go)
  :general
  (dogears-list-mode-map
   :states 'normal
   "RET" 'dogears-list-go
   "d d" 'dogears-list-delete)
  :init
  (evil-ex-define-cmd "fa" 'dogears-list)
  (evil-ex-define-cmd "fr" 'dogears-remember)
  (evil-ex-define-cmd "ff" 'dogears-go)
  (evil-ex-define-cmd "fn" 'dogears-forward)
  (evil-ex-define-cmd "fp" 'dogears-back)
  :config
  (dogears-mode)
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'dogears-list))
  ;; places to remember
  (add-to-list 'dogears-hooks 'xref-after-jump-hook)
  (add-to-list 'dogears-hooks 'bookmark-after-jump-hook)
  (add-to-list 'dogears-functions 'set-marker))

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
  :ensure t
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

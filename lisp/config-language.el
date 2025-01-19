;;; config-language.el --- Major mode packages, et al  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Pang Tun Jiang

;; Author: Pang Tun Jiang <mail@pangt.dev>

;; Also known as `emacs-lisp-mode'
(use-package elisp-mode
  :ensure nil
  :after evil
  :custom
  ;; (lisp-indent-function #'common-lisp-indent-function)
  (lisp-indent-function #'lisp-indent-function)
  :init
  (cl-defun --update-emacs-lisp-mode-line-name ()
    "Lifted from the emacs-lisp define-derived-mode. With some edits to the face
in dynamic mode."
    (setq mode-name
          `("ELisp"
            (lexical-binding (:propertize "/lex"
                              help-echo "Using lexical-binding mode")
                             (:propertize "/dyn"
                              help-echo "Using old dynamic scoping mode\n\
mouse-1: Enable lexical-binding mode"
                              face default
                              mouse-face mode-line-highlight
                              local-map ,elisp--dynlex-modeline-map)))))
  :hook (;; (emacs-lisp-mode-hook . update-evil-shift-width)
         (emacs-lisp-mode-hook . --update-emacs-lisp-mode-line-name)))

;; NOTE: We want to carefully override this
;; https://old.reddit.com/r/emacs/comments/d7x7x8/finally_fixing_indentation_of_quoted_lists/
;;
;; https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned+
;; https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94+

(with-eval-after-load "lisp-mode"
  (defun lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
,* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
,* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
,* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (let ((normal-indent (current-column))
          (orig-point (point)))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond
       ;; car of form doesn't seem to be a symbol, or is a keyword
       ((and (elt state 2)
             (or (not (looking-at "\\sw\\|\\s_"))
                 (looking-at ":")))
        (if (not (> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp))
            (progn (goto-char calculate-lisp-indent-last-sexp)
                   (beginning-of-line)
                   (parse-partial-sexp (point)
                                       calculate-lisp-indent-last-sexp 0 t)))
        ;; Indent under the list or under the first sexp on the same
        ;; line as calculate-lisp-indent-last-sexp.  Note that first
        ;; thing on that line has to be complete sexp since we are
        ;; inside the innermost containing sexp.
        (backward-prefix-chars)
        (current-column))
       ((and (save-excursion
               (goto-char indent-point)
               (skip-syntax-forward " ")
               (not (looking-at ":")))
             (save-excursion
               (goto-char orig-point)
               (looking-at ":")))
        (save-excursion
          (goto-char (+ 2 (elt state 1)))
          (current-column)))
       (t
        (let ((function (buffer-substring (point)
                                          (progn (forward-sexp 1) (point))))
              method)
          (setq method (or (function-get (intern-soft function)
                                         'lisp-indent-function)
                           (get (intern-soft function) 'lisp-indent-hook)))
          (cond ((or (eq method 'defun)
                     (and (null method)
                          (> (length function) 3)
                          (string-match "\\`def" function)))
                 (lisp-indent-defform state indent-point))
                ((integerp method)
                 (lisp-indent-specform method state
                                       indent-point normal-indent))
                (method
                 (funcall method indent-point state)))))))))

;; (emacs-lisp-mode-hook . (lambda () (setq-local comment-begin ";; "))))) ;; not working for some reason
;; (emacs-lisp-mode-hook . (lambda ()
;;                      (mapc (lambda (pair) (push pair
;;                                            prettify-symbols-alist
;;                            '(("nil"      . #x2205)
;;                              ("not"      . #xac)
;;                              ("<="       . #x2264)
;;                              (">="       . #x2265)
;;                              ;; ("defun" . #x0192)
;;                              ("or"       . #x2228)
;;                              ("and"      . #x2227))))

(use-package racket-mode
  :ensure (:host github :repo "greghendershott/racket-mode")
  :commands (racket-mode)
  :mode "\\.rkt\\'"
  :init
  (with-eval-after-load 'org-src
    (cl-pushnew '("rkt" . racket) org-src-lang-modes)))

(use-package clojure-mode
  :ensure (:host github :repo "clojure-emacs/clojure-mode")
  :after evil
  :commands (clojure-mode
             clojurescript-mode)
  :hook ((clojure-mode-hook . update-evil-shift-width)
         (clojure-mode-hook . show-paren-mode))
  :init
  (with-eval-after-load 'org-src
    (cl-pushnew '("edn" . clojure) org-src-lang-modes)
    (cl-pushnew '("clj" . clojure) org-src-lang-modes)
    (cl-pushnew '("cljs" . clojurescript) org-src-lang-modes)))

(use-package rust-mode
  :ensure (:host github :repo "rust-lang/rust-mode")
  :mode
  ("\\.rs\\'" . rust-mode)
  :custom
  (rust-format-show-buffer nil
                           "Stop polluting my workspace with orphaned
                           windows thanks")
  (rust-format-on-save t)
  :general
  (:states 'insert
   :keymaps 'rust-mode-map
   "RET" 'comment-indent-new-line)
  :init
  (with-eval-after-load 'org-src
    (cl-pushnew '("rust" . rust) org-src-lang-modes))
  :config
  (with-eval-after-load 'treesit
    (setq rust-mode-treesitter-derive t)))

;; treesit
(with-eval-after-load 'rust-ts-mode
  (with-eval-after-load 'general
    (general-define-key
     :keymaps 'rust-ts-mode-map
     :states 'insert
     "RET" 'comment-indent-new-line)

    (general-define-key
     :keymaps 'rust-ts-mode-map
     :states '(insert normal visual)
     "C-c C-d" 'rust-dbg-wrap-or-unwrap))

  ;; So that `compile' will correctly color/link to rustc errors
  (require 'rust-compile)

  (with-eval-after-load 'rust-mode
    (setq rust-ts-mode-hook rust-mode-hook))

  (cl-defun --rust-ts-mode-rustfmt ()
    "Rustfmts buffer before saving."
    (require 'rust-rustfmt)
    (add-hook 'before-save-hook 'rust-format-buffer nil t))

  (add-hook 'rust-ts-mode-hook '--rust-ts-mode-rustfmt))

(use-package cargo
  :ensure (:host github :repo "kwrooijen/cargo.el")
  :blackout t
  :commands cargo-minor-mode
  :hook (rust-mode-hook . cargo-minor-mode))

(use-package cargo-mode
  :ensure (:host github :repo "ayrat555/cargo-mode")
  :commands
  (cargo-mode-execute-task
   cargo-mode-test
   cargo-mode-last-command
   cargo-mode-build
   cargo-mode-test-current-buffer
   cargo-mode-test-current-test))

(use-package json-mode
  :ensure (:host github :repo "joshwnj/json-mode")
  :mode "\\.json\\'"
  :commands (json-mode)
  :init
  (with-eval-after-load 'org-src
    (cl-pushnew '("json" . json) org-src-lang-modes))
  :config
  (with-eval-after-load 'smartparens
    (sp-local-pair 'json-mode "{" nil :post-handlers '((--double-newline-and-indent-braces "RET")))
    (sp-local-pair 'json-mode "[" nil :post-handlers '((--double-newline-and-indent-braces "RET")))
    (sp-local-pair 'json-mode "(" nil :post-handlers '((--double-newline-and-indent-braces "RET")))))

(use-package json-snatcher
  :after json-mode
  :commands (jsons-print-path))

(use-package yaml-mode
  :mode ("\\.yml\\'"
         "\\.yaml\\'"
         "\\.yaml.in\\'")
  :hook
  (yaml-mode-hook . display-line-numbers-mode)
  :general
  (yaml-mode-map
   "RET" 'newline-and-indent)
  :init
  (with-eval-after-load 'org-src
    (cl-pushnew '("yaml" . yaml) org-src-lang-modes))
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'yaml-mode-hook #'flycheck-mode))

  (with-eval-after-load 'highlight-indent-guides
    (add-hook 'yaml-mode-hook #'highlight-indent-guides-mode))

  (with-eval-after-load 'smartparens
    (sp-local-pair 'yaml-mode "{" nil :post-handlers '((--double-newline-and-indent-braces "RET")))
    (sp-local-pair 'yaml-mode "[" nil :post-handlers '((--double-newline-and-indent-braces "RET")))
    (sp-local-pair 'yaml-mode "(" nil :post-handlers '((--double-newline-and-indent-braces "RET")))))

(use-package yaml-ts-mode
  :ensure nil
  :if (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  :commands (yaml-ts-mode)
  :after yaml-mode
  :config
  (setq yaml-ts-mode-hook yaml-mode-hook)
  (with-eval-after-load 'flycheck
    (add-hook 'yaml-ts-mode-hook #'flycheck-mode))
  (with-eval-after-load 'highlight-indent-guides
    (add-hook 'yaml-ts-mode-hook #'highlight-indent-guides-mode)))

;; https://github.com/zkry/yaml.el/tree/9ebddb55238d746dc5a5d46db04c9f360c140b99
(use-package yaml
  :after yaml-mode
  :commands (yaml-parse-string))

(use-package cc-mode
  :ensure nil
  :general
  (:states 'insert
   :keymaps 'c-mode-base-map
   "C-<return>" 'comment-indent-new-line)
  (:keymaps '(c-mode-map c++-mode-map)
    "<tab>" #'--c-indent-then-complete
    "TAB" "<tab>")
  :init
  (cl-defun --c-indent-then-complete ()
    (interactive)
    (when (= 0 (c-indent-line-or-region))
      (completion-at-point)))
  :config
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))

  (setq-default c-basic-offset 4)

  (cl-dolist (mode '(c-mode c++-mode))
    (add-to-list 'c-default-style `(,mode . "k&r")))

  (with-eval-after-load 'c-ts-mode
    (setq c-ts-mode-indent-offset 4))

  ;; We do not support =&&= (and hence also =||=) because of conflicts with
  ;; r-value references.
  (add-hook 'c-mode-common-hook
            #'(lambda ()
                (mapc (lambda (pair) (push pair prettify-symbols-alist))
                      '(("!=" . #x2260)
                        ("!" . #xac)
                        ;; ("==" . #x2a75) ;; Not supported by Iosevka
                        ;; ("->" . #x27f6) ;; Not supported by Iosevka
                        ("->" . #x2192) ;; 1-char width version
                        ("<=" . #x2264)
                        (">=" . #x2265))))))
;; ("&&" . #x2227)
;; ("||" . #x2228)))))

(use-package clang-format
  :ensure (:host github :repo "emacsmirror/clang-format")
  :commands (clang-format-region
             clang-format-buffer
             clang-format)
  :general
  (c++-mode-map
   :states '(normal)
   "C-c C-f" 'my-evil-clang-format)
  (java-mode-map
   :states '(normal)
   "C-c C-f" 'my-evil-clang-format)
  :init
  (with-eval-after-load 'evil
    (evil-define-operator my-evil-clang-format (beg end)
      (require 'clang-format)
      (clang-format beg end)))

  (cl-defun toggle-clang-format-on-save ()
    "Toggle clang-format-buffer on a per-buffer level."
    (interactive)
    (unless (boundp 'toggle-clang-format-on-save)
      (setq-local toggle-clang-format-on-save nil))
    (if (not toggle-clang-format-on-save)
        (progn (add-hook 'before-save-hook 'clang-format-buffer t t)
               (setq-local toggle-clang-format-on-save t)
               (message "toggle-clang-format-on-save t"))
      (remove-hook 'before-save-hook 'clang-format-buffer t)
      (setq-local toggle-clang-format-on-save nil)
      (message "toggle-clang-format-on-save nil")))
  :custom
  (clang-format-style-option "file"
                             "read from .clang-format"))

(use-package clang-format+
  :after clang-format
  :commands clang-format+-mode)

;; Requires `clang` to be installed (perhaps unsurprisingly).
(use-package clang-capf
  :custom
  (clang-capf-ignore-case t)
  :config
  (add-hook 'c-mode-hook
            #'(lambda ()
                (add-hook 'completion-at-point-functions #'clang-capf nil t))))

(use-package beancount
  :ensure (:host github :repo "beancount/beancount-mode")
  :mode (("\\.beancount\\'" . beancount-mode)
         ("\\.ledger\\'" . beancount-mode))
  :init
  (cl-defun my-beancount-insert-date ()
    "Does the same thing as `beancount-insert-date', but uses
`org-read-date' for more ergonomic date generation."
    (interactive)
    (unless (bolp) (newline)) ;; `beancount-insert-date' does this
    (insert (org-read-date) " "))
  ;; TODO: Add more from
  ;; `https://beancount.github.io/docs/beancount_language_syntax.html`
  (when (bound-and-true-p --default-ledger-file)
    (with-eval-after-load 'org-capture
      (setq org-capture-templates
            (doct-add-to
             org-capture-templates
             '(("Beancount"
                :keys "bean"
                ;; :contexts (:in-mode "beancount-mode")
                :type plain
                :file --default-ledger-file
                :unnarrowed t ;; critical for completion to function
                :empty-lines-before 1
                :children
                (("Today"
                  :keys "today"
                  :children
                  (("Open new account"
                    :keys "open"
                    :template
                    ("%<%Y-%m-%d> open %? %^{CURRENCY}"))
                   ("Transaction"
                    :keys "tx"
                    :template
                    ("%<%Y-%m-%d> * \"%^{VENDOR}\" \"%^{PURCHASE}\""
                     "  %?"))))
                 ("On Date"
                  :keys "date"
                  :date (lambda () (org-read-date))
                  :children
                  (("Open new account"
                    :keys "open"
                    :template
                    ("%{date} open %? %^{CURRENCY}"))
                   ("Transaction"
                    :keys "tx"
                    :template
                    ("%{date} * \"%^{VENDOR}\" \"%^{PURCHASE}\""
                     "  %?")))))))))))
  (with-eval-after-load 'evil
    (--evil-ex-define-cmds-splits-and-tabs
            "ledger"
            #'(lambda () (interactive)
                (find-file --default-ledger-file))))
  :general
  (beancount-mode-map
   "M-RET" #'my-beancount-insert-date)
  :hook
  (beancount-mode-hook . outline-minor-mode)
  :config
  (with-eval-after-load 'org-src
    (cl-pushnew '("beancount" . beancount) org-src-lang-modes)))
;; :general
;; (beancount-mode-map
;;  :states '(normal motion insert)
;;  "<f5>" #'(lambda () (interactive)
;;            (insert (format-time-string "%Y-%m-%d"))))
;; causing fontification errors
;; :hook ((org-mode-hook . beancount-mode)))

(use-package groovy-mode
  :ensure (:host github :repo "Groovy-Emacs-Modes/groovy-emacs-modes")
  :mode (("\\.groovy\\'" . groovy-mode)
         ("\\Jenkinsfile\\'" . groovy-mode))
  :hook ((groovy-mode-hook . (lambda () (setq-local comment-start "// "))))
  :init
  (with-eval-after-load 'org-src
    (cl-pushnew '("groovy" . groovy) org-src-lang-modes)))

(use-package hy-mode
  :ensure (:host github :repo "hylang/hy-mode")
  :mode "\\.hy\\'"
  :after evil
  :general
  (hy-mode-map
   :states 'insert
   "RET" 'comment-indent-new-line)
  :hook ((hy-mode-hook . update-evil-shift-width)
         (hy-mode-hook . show-paren-mode))
  :init
  (with-eval-after-load 'org-src
    (cl-pushnew '("hy" . hy) org-src-lang-modes)))

(use-package ob-hy
  :ensure (:host github :repo "brantou/ob-hy")
  :after (:all org-src hy-mode))

(use-package python
  :ensure nil
  :general
  (:keymaps 'python-mode-map
   :states 'insert
   "RET" 'comment-indent-new-line)
  :config
  (setq-default python-indent-offset 4)
  (with-eval-after-load 'org
   (org-babel-do-load-languages 'org-babel-load-languages
                                (add-to-list 'org-babel-load-languages '(python . t)))))

(use-package pyvenv
  :after python
  :ensure (:host github :repo "jorgenschaefer/pyvenv")
  :commands (pyvenv-activate
             pyvenv-workon))

(use-package powershell.el
  :ensure (:host github :repo "jschaf/powershell.el")
  :commands (powershell-mode powershell)
  :init
  (with-eval-after-load 'org-src
    (cl-pushnew '("powershell" . powershell) org-src-lang-modes)
    (cl-pushnew '("ps" . powershell) org-src-lang-modes))
  :config
  (defun org-babel-execute:powershell (body _params)
    (let ((explicit-shell-file-name powershell-location-of-exe))
      (shell-command-to-string body))))

(use-package cmake-mode
  :mode ("\\cmakelists.txt\\'" . cmake-mode)
  :hook (cmake-mode-hook . hl-todo-mode))

(use-package cmake-font-lock
  :after cmake-mode
  :hook (cmake-mode-hook . cmake-font-lock-activate))

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :hook
  (go-mode-hook . gofmt-before-save)
  :general
  (:keymaps 'go-mode-map
   :state 'normal
   "C-c C-f C-f" 'gofmt))

(with-eval-after-load 'org-src
 (cl-pushnew '("cmd" . bat) org-src-lang-modes)
 (cl-pushnew '("batch" . bat) org-src-lang-modes)
 (cl-pushnew '("toml" . conf-toml) org-src-lang-modes))

(use-package plantuml-mode
  :ensure (:host github :repo "skuro/plantuml-mode")
  :commands (plantuml-mode)
  :custom
  (plantuml-output-type "txt"
                        "Display using glorious text")
  (plantuml-default-exec-mode 'executable)
  :mode "\\.plantuml\\'"
  :init
  (with-eval-after-load 'org-src
    (cl-pushnew '("plantuml" . plantuml) org-src-lang-modes)))

;; http://plantuml.com/index
(with-eval-after-load 'org
  (customize-set-value 'org-plantuml-exec-mode 'plantuml)
  (org-babel-do-load-languages 'org-babel-load-languages
                               (add-to-list 'org-babel-load-languages '(plantuml . t))))

(use-package edn
  :ensure (:host github :repo "expez/edn.el")
  :commands (edn-read
             edn-print-string))

(use-package web-mode
  :mode (("\\.svelte\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  :commands web-mode
  :config
  (with-eval-after-load 'lsp-mode
    (add-hook 'web-mode-hook #'lsp-deferred)))

(use-package moonscript)

(use-package hcl-mode)

(use-package terraform-mode
  :hook (terraform-mode-hook . terraform-format-on-save-mode)
  :config
  (with-eval-after-load 'tree-sitter
    (add-hook 'terraform-mode-hook #'tree-sitter-mode)))

(use-package dockerfile-mode)

;; multi major mode
(use-package mmm-mode
  :custom-face
  (mmm-default-submode-face ((t (:background unspecified)))))

;; objc
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

(use-package typescript-mode
  :disabled t
  :mode "\\.ts\\'"
  :config
  (with-eval-after-load 'tree-sitter
    (add-hook 'typescript-mode-hook 'tree-sitter-hl-mode)))

;; Requires `mermaid-cli' to be installed
;; ```
;; npm install -g @mermaid-js/mermaid-cli
;; ```
(use-package mermaid-mode
  :if (and (bound-and-true-p --mermaid-cli-bin)
           (f-exists-p --mermaid-cli-bin))
  :custom
  (mermaid-mmdc-location --mermaid-cli-bin)
  :commands (mermaid-mode)
  :init
  (with-eval-after-load 'markdown-mode
    (add-to-list 'markdown-code-lang-modes '("mermaid" . mermaid-mode))))

(use-package ob-mermaid
  :after (org mermaid-mode)
  :if (and (bound-and-true-p --mermaid-cli-bin)
           (f-exists-p --mermaid-cli-bin))
  :custom
  (ob-mermaid-cli-path --mermaid-cli-bin))

(use-package kotlin-mode
  :mode ("\\.kt\\'" . kotlin-mode))

(use-package swift-mode
  :mode ("\\.swift\\'" . swift-mode))

(use-package earthfile-mode)

;; asciidoc
(use-package adoc-mode
  :commands adoc-mode)

(use-package jam-mode
  :ensure nil
  :mode "Jamroot"
  :load-path "lisp/3p"
  :init
  (cl-defun --jamroot-imenu-index-matcher (menu-title rx-matcher)
    "Very simple, very stupid way of generating regexes for imenu in Jamroot."
    (list menu-title
          (rx bol (literal rx-matcher)
              (1+ whitespace) (group-n 1 (1+ (any alnum ?- ?_)))) 1))
  (cl-defun --update-imenu ()
    (require 'rx)
    ;; Useful reference list:
    ;; https://www.boost.org/build/tutorial.html#rule_reference

    ;; certain regexes are already predefined. We remove them and redefine our
    ;; own.
    (setq-local imenu-generic-expression nil)
    (add-to-list 'imenu-generic-expression
                 (--jamroot-imenu-index-matcher "Aliases" "alias"))
    (add-to-list 'imenu-generic-expression
                 (--jamroot-imenu-index-matcher "Libraries" "lib"))
    (add-to-list 'imenu-generic-expression
                 (--jamroot-imenu-index-matcher "Executables" "exe"))
    (add-to-list 'imenu-generic-expression
                 (--jamroot-imenu-index-matcher "Install" "install"))
    (add-to-list 'imenu-generic-expression
                 (--jamroot-imenu-index-matcher "Rules" "rule"))
    (add-to-list 'imenu-generic-expression
                 (--jamroot-imenu-index-matcher "Actions" "actions"))
    (add-to-list 'imenu-generic-expression
                 (--jamroot-imenu-index-matcher "Explicit" "explicit")))
  :hook
  (jam-mode-hook . --update-imenu))

(use-package csv-mode
  :ensure (:host github :repo "emacs-straight/csv-mode" :branch "master")
  :custom
  (csv-align-style 'auto)
  :config
  (with-eval-after-load 'stripe-buffer
    (add-hook 'csv-mode-hook 'stripe-buffer-mode)))

(use-package fish-mode)

(provide 'config-language)

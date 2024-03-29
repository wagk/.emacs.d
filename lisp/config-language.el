;;; config-language.el --- Major mode packages, et al  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Pang Tun Jiang

;; Author: Pang Tun Jiang <mail@pangt.dev>

;; Also known as `emacs-lisp-mode'
(use-package elisp-mode
  :ensure nil
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
                              face nano-subtle
                              mouse-face mode-line-highlight
                              local-map ,elisp--dynlex-modeline-map)))))
  :hook ((emacs-lisp-mode-hook . update-evil-shift-width)
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
    (cl-pushnew '("rust" . rust) org-src-lang-modes)))

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

(provide 'config-language)

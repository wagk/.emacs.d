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

(provide 'config-text)

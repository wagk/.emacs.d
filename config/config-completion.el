;;; config-completion.el --- autocompletion and snippets

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)
(require 'config-helm)

(use-package yasnippet
  :commands yas-expand-snippet
  :bind(:map yas-keymap
             ("C-j" . yas-next-field-or-maybe-expand)
             ("C-k" . yas-prev-field))
  :init
  (general-define-key :prefix my-default-evil-leader-key
                      "s s" 'yas-new-snippet
                      "s a" 'yas-insert-snippet
                      "s f" 'yas-visit-snippet-file)
  :config
  (let ((my-snippet-dir (concat user-init-dir "snippets")))
    (setq-default yas-snippet-dirs `(,my-snippet-dir)))
  (yas-global-mode)
  (setq yas-indent-line 'auto
        yas-also-auto-indent-first-line t)
  (define-key snippet-mode-map [remap evil-save-and-close]
    'yas-load-snippet-buffer-and-close)
  (define-key snippet-mode-map [remap evil-save-modified-and-close]
    'yas-load-snippet-buffer-and-close)
  (define-key snippet-mode-map [remap evil-quit]
    'kill-buffer)
  )

;; auto-insert yasnippets
;; www.howardism.org/Technical/Emacs/templates-tutorial.html
;; (setq yas-snippet-dirs (append yas-snippet-dirs ))
;;;###autoload
(defun /auto-insert-yasnippet ()
  "Replace text in buffer with snippet.
Used for 'auto-insert'"
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(setq-default auto-insert-directory (concat user-init-dir "./auto-insert/"))
(auto-insert-mode 1)
(setq-default auto-insert-query nil
              auto-insert 'other)
(define-auto-insert "\\.el$"  ["elisp-template" /auto-insert-yasnippet])
(define-auto-insert "\\.py$"  ["python-template" /auto-insert-yasnippet])
(define-auto-insert "\\.h$"   ["cpp-h-template" /auto-insert-yasnippet])
(define-auto-insert "\\.cpp$" ["cpp-template" /auto-insert-yasnippet])
(define-auto-insert "\\.sh$"  ["sh-template" /auto-insert-yasnippet])

(defun yas-with-comment (str)
  (format "%s%s%s" comment-start str comment-end))

;; this package doesn't seem to be doing anything
;; (use-package org-sync-snippets
;;   :ensure t
;;   :config
;;   (progn (require 'org)
;;          (add-hook 'yas-after-reload-hook 'org-sync-snippets-snippets-to-org)
;;          )
;;   )

(use-package company
  :bind(
        ;; :map evil-insert-state-map
        ;;      ("C-p" . company-complete)
        ;;      ("C-n" . company-complete)
        :map company-active-map
             ("C-j" . company-select-next)
             ("C-k" . company-select-previous)
             ("C-w" . evil-delete-backward-word))
  :hook (prog-mode . company-mode)
  :config
  (global-company-mode)
  ;; yasnippet integration
  ;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
  (progn (require 'yasnippet)
         (defvar company-mode/enable-yas t
           "Enable yasnippet for all backends.")
         (defun company-mode/backend-with-yas (backend)
           (if (or (not company-mode/enable-yas)
                   (and (listp backend)
                        (member 'company-yasnippet backend)))
               backend
             (append (if (consp backend)
                         backend
                       (list backend))
                     '(:with company-yasnippet))))
         (setq company-backends
               (mapcar #'company-mode/backend-with-yas
                       company-backends))
         )
  ;; fci-mode makes the completion popup spaz.
  ;; this is an attempted workaround
  ;; https://github.com/company-mode/company-mode/issues/180
  (progn (defvar-local company-fci-mode-on-p nil)
         (defun company-turn-off-fci (&rest ignore)
           (when (boundp 'fci-mode)
             (setq company-fci-mode-on-p fci-mode)
             (when fci-mode (fci-mode -1))))

         (defun company-maybe-turn-on-fci (&rest ignore)
           (when company-fci-mode-on-p (fci-mode 1)))

         (add-hook 'company-completion-started-hook 'company-turn-off-fci)
         (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
         (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
         )
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-idle-delay 0.5
        company-require-match nil
        company-selection-wrap-around t)
  )

;; (use-package helm-company
;;   :config
;;   (evil-declare-key 'insert company-mode-map (kbd "C-SPC") 'helm-company)
;;   (evil-declare-key 'insert company-active-map (kbd "C-SPC") 'helm-company))

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode 0)
  (setq company-quickhelp-delay 1))

;; (use-package company-jedi
;;   :config
;;   (require 'company)
;;   (add-hook 'python-mode-hook 'company-jedi)
;;   )

;; (use-package auto-insert
;;   :ensure t
;;   )

(add-hook 'prog-mode-hook #'(lambda () (abbrev-mode -1)))

(provide 'config-completion)

;;; config-completion.el ends here

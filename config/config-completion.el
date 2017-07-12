;;; config-completion.el --- autocompletion and snippets

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)
(require 'config-helm)

(use-package yasnippet
  :ensure t
  :bind(:map yas-keymap
             ("C-j" . yas-next-field-or-maybe-expand)
             ("C-k" . yas-prev-field))
  :config
  (yas-global-mode 1))

(use-package company
  :ensure t
  :bind(:map evil-insert-state-map
             ("C-p" . company-complete)
             ("C-p" . company-complete)
             ("C-n" . company-complete)
             :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-w" . evil-delete-backward-word))
  :config
  (global-company-mode)

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
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-idle-delay 0
        company-require-match nil
        company-selection-wrap-around t))

;; (use-package helm-company
;;   :config
;;   (evil-declare-key 'insert company-mode-map (kbd "C-SPC") 'helm-company)
;;   (evil-declare-key 'insert company-active-map (kbd "C-SPC") 'helm-company))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 0)
  (setq company-quickhelp-delay 1))

;; (use-package company-jedi
;;   :config
;;   (require 'company)
;;   (add-hook 'python-mode-hook 'company-jedi)
;;   )

(provide 'config-completion)

;;; config-completion.el ends here

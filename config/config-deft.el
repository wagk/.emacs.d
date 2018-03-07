;;; config-deft.el --- deft configuration

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)

;; https://jblevins.org/projects/deft/
(use-package deft
  :commands (deft)
  :custom
  (deft-directory (concat /dropbox-folder "/notes")
    "Set the directory to dropbox")
  (deft-extensions '("org" "md")
    "Set the extensions for deft notes")
  (deft-use-filename-as-title t)
  :init
  (general-define-key :prefix my-default-evil-leader-key
                      "n" 'deft)
  :config
  ;; (evil-make-overriding-map deft-mode-map nil)
  (evil-set-initial-state 'deft-mode 'insert)
  ;; I wonder why evil keeps overriding RET with evil-ret
  ;; (general-define-key :states '(insert motion normal)
  ;;                     :keymaps 'deft-mode-map
  ;;                     "RET" 'deft-complete)
  (add-hook 'deft-open-file-hook 'org-mode)
  (define-key deft-mode-map [remap evil-quit]
    'kill-this-buffer)
  (define-key deft-mode-map [remap evil-save-modified-and-close]
    'kill-this-buffer)
  ;; TODO: See if this method can be applied to eshell hacks
  ;; TODO: This isn't working for some reason
  (define-key deft-mode-map [remap evil-ret]
    'deft-complete)
  ;; (defun my-overwrite-evil-ret-in-deft ()
  ;;   "attempts to make evil-ret in deft do things like send input"
  ;;   (message "Attempting to overwrite RET for deft")
  ;;   ;; (with-eval-after-load 'evil-config
  ;;   ;;   (define-))
  ;;   (evil-local-set-key 'insert
  ;;                       (kbd "RET") 'deft-complete)
  ;;   (evil-local-set-key 'normal
  ;;                       (kbd "RET") 'deft-complete)
  ;;   (evil-local-set-key 'motion
  ;;                       (kbd "RET") 'deft-complete)
  ;;   )
  ;; (add-hook 'deft-mode-hook 'my-overwrite-evil-ret-in-deft)
  )

(provide 'config-deft)

;;; config-deft.el ends here

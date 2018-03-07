;;; config-deft.el --- deft configuration

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)

;; https://jblevins.org/projects/deft/
(use-package deft
  :demand t
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
  ;; TODO: find a way to call a command with saved arguments, we want
  ;; `kill-buffer' to NOT bring up a helm search everytime we call it
  ;; TODO: See if this method can be applied to eshell hacks
  (define-key deft-mode-map [remap evil-quit]
    'kill-buffer)
  (define-key deft-mode-map [remap evil-save-modified-and-close]
    'kill-buffer)
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

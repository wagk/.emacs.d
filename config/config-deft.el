;;; config-deft.el --- deft configuration

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)

;; https://jblevins.org/projects/deft/
(use-package deft
  :commands (deft)
  :custom
  (deft-auto-save-interval 0.0 "Disable autosave because of permissions issues")
  (deft-directory (concat /dropbox-folder "/notes")
    "Set the directory to dropbox")
  (deft-extensions '("org" "md")
    "Set the extensions for deft notes")
  (deft-use-filter-string-for-filename t)
  (deft-file-naming-rules '((noslash . "-")
                            (nospace . "-")
                            (case-fn . downcase)))
  (deft-org-mode-title-prefix t)
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
  (add-hook 'deft-mode-hook #'(lambda ()
                                (require 'org)
                                (add-to-list 'org-agenda-files deft-directory)))
  (add-hook 'deft-open-file-hook 'org-mode)
  (general-define-key :keymaps 'deft-mode-map
                      :states '(insert normal motion)
                      "C-j" 'widget-forward
                      "C-k" 'widget-backward)
  (general-define-key :keymaps 'deft-mode-map
                      :states 'normal
                      "p" 'deft-filter-yank
                      "d d" 'deft-delete-file)
  (general-define-key :states 'insert
                      :keymaps 'deft-mode-map
                      "C-w" 'deft-filter-decrement-word
                      "C-u" 'deft-filter-clear)
  ;; (define-key deft-mode-map [remap evil-quit]
  ;;   'kill-this-buffer)
  ;; (define-key deft-mode-map [remap evil-save-modified-and-close]
  ;;   'kill-this-buffer)
  ;; TODO: See if this method can be applied to eshell hacks
  ;; TODO: This isn't working for some reason
  ;; (define-key deft-mode-map [remap evil-ret]
  ;;   'deft-complete)
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

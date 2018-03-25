;;; config-visualise.el --- pdfs and images and all that

;;; Commentary:

;;; Code:
(require 'config-package)

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  ;; :config
  ;; (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  )

;;TODO: Configure this. Evil is shadowing most of the binds
;; https://github.com/TatriX/pomidor
(use-package pomidor
  :general
  (:states 'normal
   :prefix my-default-evil-leader-key
   "c" 'pomidor)
  :config
  (evil-set-initial-state 'pomidor-mode 'emacs)
  :custom
  (pomidor-default-alert 'mode-line))

(provide 'config-visualise)

;;; config-visualise.el ends here

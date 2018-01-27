;;; config-cpp.el --- c/cpp/c#/c-family configurations

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-buffer)

(use-package irony
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :config
  (setq w32-pipe-read-delay 0))

(use-package company-irony
  :after company
  :defer nil
  :config
  (add-to-list 'company-backends 'company-irony)
  )

;; treat .h files as cpp files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; gnu indent style is mildly retarded
(setq-default c-default-style "k&r")

;; we don't electric pair <> because it interferes with << operators

;; ;; add < > electric pairing
;; (defvar $c++-electric-pairs '((?< . ?>))
;;   "Additional electric pairs for c++")

;; (defun $c++-mode-add-pairs ()
;;   (setq-local electric-pair-pairs (append electric-pair-pairs
;;                                           $c++-electric-pairs))
;;   (setq-local electric-pair-text-pairs electric-pair-pairs))

;; (add-hook 'c++-mode-hook #'$c++-mode-add-pairs)

(use-package cmake-mode
  :mode ("\\cmakelists.txt\\'" . cmake-mode)
  :config
  (add-hook 'cmake-mode-hook 'hl-todo-mode)
  )

(use-package cmake-font-lock
  :after cmake-mode
  :demand t)

(provide 'config-cpp)

;;; config-cpp.el ends here

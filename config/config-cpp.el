;;; config-cpp.el --- c/cpp/c#/c-family configurations

;;; Commentary:

;;; Code:
(require 'config-package)

(use-package irony
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :config
  (setq w32-pipe-read-delay 0))

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

(provide 'config-cpp)

;;; config-cpp.el ends here

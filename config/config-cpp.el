;;; config-cpp.el --- c/cpp/c#/c-family configurations

;;; Commentary:

;;; Code:
(require 'config-package)

(use-package irony
  :ensure t
  :config
  (setq w32-pipe-read-delay 0)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; treat .h files as cpp files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; add < > electric pairing
(defvar $c++-electric-pairs '((?< . ?>))
  "Additional electric pairs for c++")

(defun $c++-mode-add-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs
                                          $c++-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(add-hook 'c++-mode-hook #'$c++-mode-add-pairs)

(provide 'config-cpp)

;;; config-cpp.el ends here

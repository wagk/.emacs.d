;;; config-cpp.el --- c/cpp/c#/c-family configurations

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-buffer)
(require 'config-project)
(require 'config-indent)

;; We are disabling this for the moment because the irony server is crashing all
;; the time and the input lag is annoying
(use-package irony
  :disabled t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :config
  ;; Windows performance tweaks
  ;;
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  ;; Set the buffer size to 64K on Windows (from the original 4K)
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
  )

(use-package company-irony
  :disabled t
  :after (:all company irony)
  :defer nil
  :config
  (add-to-list 'company-backends 'company-irony)
  )

;; treat .h files as cpp files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; gnu indent style is mildly retarded
(setq-default c-default-style "k&r"
              c-basic-offset 4)

(defun my-cpp-mode-configs ()
  "Configurations for c++-mode, since it doesn't have"
  (setq tab-width 4)
  (with-eval-after-load 'flycheck
    (defun my-disable-flycheck-clang-checker ()
      ;;We disable the clang checker for pretty much the same reason we disabled
      ;;irony
      (add-to-list 'flycheck-disabled-checkers 'c/c++-clang))
    (add-hook 'flycheck-mode-hook 'my-disable-flycheck-clang-checker)
    (flycheck-mode -1))
  (with-eval-after-load 'company
    (make-local-variable 'company-backends)
    (let ((curr-backends company-backends)
          (new-backends (list)))
      (dolist (backend curr-backends)
        (unless (equal (car backend) 'company-clang)
          (add-to-list 'new-backends backend)))
      (setq company-backends new-backends))
    (company-mode -1)
    (global-company-mode -1))
  )

(add-hook 'c++-mode-hook 'my-cpp-mode-configs)

(general-define-key :states 'insert
                    :keymaps 'c-mode-base-map
                    "RET" 'comment-indent-new-line)


;; we don't electric pair <> because it interferes with << operators

;; ;; add < > electric pairing
;; (defvar $c++-electric-pairs '((?< . ?>))
;;   "Additional electric pairs for c++")

;; (defun $c++-mode-add-pairs ()
;;   (setq-local electric-pair-pairs (append electric-pair-pairs
;;                                           $c++-electric-pairs))
;;   (setq-local electric-pair-text-pairs electric-pair-pairs))

;; (add-hook 'c++-mode-hook #'$c++-mode-add-pairs)

;; make sure that this is running clang-format 7 or something. A newer version
(use-package clang-format
  :commands (clang-format-region
             clang-format-buffer
             clang-format)
  :init
  ;; IF there is a .clang-format, then use that to format before saving
  (defun my-clang-format-before-save ()
    (require 'projectile)
    (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
      (add-hook 'before-save-hook 'clang-format-buffer t t)))
  (add-hook 'c++-mode-hook #'my-clang-format-before-save)
  ;; (add-to-list 'aggressive-indent-excluded-modes 'c++-mode)
  :custom
  (clang-format-style-option "file"
                             "read from .clang-format"))

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

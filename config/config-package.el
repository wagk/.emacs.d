;;; config-package.el --- Manage package loading and sources

;;; Commentary:

;;; Code:

;; Setup package stuff
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-2" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")) ; https://marmalade-repo.org/packages/#windowsinstructions

;; el-get stuff
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

; Got a warning regarding golden-ratio when I loaded this before el-get, for
;; some reason
;; TODO; figure out what this does
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(progn (eval-when-compile (require 'use-package)))

;; download packages if needed
;; this is disabled because I feel that verbose is better
;; (setq use-package-always-ensure t)
(setq use-package-always-defer t ;; always lazy load
      use-package-always-ensure t ;; always make sure it never skips if not found
      use-package-verbose t
      use-package-compute-statistics t)

(use-package diminish)
(use-package bind-key)
(use-package general
  :demand t
  :commands (general-define-key)
  :init
  (defconst my-default-evil-leader-key "SPC")
  )

(use-package use-package-el-get
  :demand t
  :config
  (use-package-el-get-setup))

(use-package use-package-ensure-system-package
  :demand t)

;; ;; be aware that updates might adjust the load path to the .el files and
;; ;; cause loading problems. Helm seems to be a victim of this a lot
;; (use-package spu
;;   :disabled t
;;   :defer 5 ;; defer package loading for 5 second
;;   :config
;;   ;; attempt to upgrade packages only when we're leaving
;;   (add-hook 'kill-emacs-hook 'spu-package-upgrade))

;; https://github.com/emacscollective/auto-compile
(use-package auto-compile
  :demand t
  :init
  (setq load-prefer-newer t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  )

(use-package auto-package-update
  :demand t
  :commands (auto-package-update-now
             auto-package-update-at-time
             auto-package-update-maybe)
  :custom
  (auto-package-update-delete-old-versions t
                                           "We already version them on git")
  (auto-package-update-prompt-before-update t
                                            "NO SURPRISES")
  (auto-package-update-interval 14
                                "update once every 2 weeks (the count is in days)"))

(provide 'config-package)

;;; config-package.el ends here

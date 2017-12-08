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

;; be aware that updates might adjust the load path to the .el files and
;; cause loading problems. Helm seems to be a victim of this a lot
(use-package spu
  :defer 5 ;; defer package loading for 5 second
  :config
  ;; attempt to upgrade packages only when we're leaving
  (add-hook 'kill-emacs-hook 'spu-package-upgrade))

;; https://github.com/emacscollective/auto-compile
(use-package auto-compile
  :demand t
  :init
  (setq load-prefer-newer t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  )

;; ;; el-get stuff
;; (add-to-list 'load-path
;;              (concat user-init-dir "/el-get/el-get"))

;; (unless (require 'el-get nil 'noerror)
;;   (package-install 'el-get)
;;   (require 'el-get))

;; (eval-after-load 'el-get
;;   '(progn
;;      (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;;      (el-get 'sync)))


(provide 'config-package)

;;; config-package.el ends here

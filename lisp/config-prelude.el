;; Load core configuration that I can't work without.

;; https://github.com/magnars/dash.el
(use-package dash :straight t)

;; https://github.com/plexus/a.el/
;; alist functions
(use-package a :straight t)

;; https://github.com/rejeep/f.el/
;; file functions
(use-package f :straight t)

;; https://github.com/magnars/s.el/
;; string functions
(use-package s :straight t)

;; https://github.com/Wilfred/ht.el/
;; hash-table functions
(use-package ht :straight t)

;; https://github.com/alphapapa/ts.el
;; date and time functions
(use-package ts :straight t)

(use-package async :straight t)

(use-package general
  :straight (:host github :repo "noctuid/general.el")
  :init
  (defconst my-default-evil-leader-key "SPC"))

(use-package no-littering
  :straight (:host github :repo "emacscollective/no-littering"))

(use-package restart-emacs
  :if (not (eq system-type 'darwin))
  :straight (:host github :repo "iqbalansari/restart-emacs")
  :commands (restart-emacs restart-emacs-start-new-emacs)
  :init
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "restart" 'restart-emacs)
    (evil-ex-define-cmd "restarttest"
                        'restart-emacs-start-new-emacs)))

(use-package blackout :straight t)

(let ((custom (locate-user-emacs-file "custom.el")))
  (unless (f-exists-p custom)
    (f-touch custom))
  (setq custom-file custom))

(provide 'config-prelude)

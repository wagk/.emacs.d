(require 'use-package)
(require 'config-evil)

(use-package dired
  :demand t
  :ensure nil
  :straight nil
  :general
  (dired-mode-map
   :states 'normal
   "<SPC>" nil                     ; was shadowing leader key bindings
   "SPC" nil                       ; was shadowing leader key bindings
   "-" 'dired-up-directory
   "d" 'dired-create-directory
   "e" 'dired-toggle-read-only ; similar interface to wgrep
   "i" nil ; unbind the original binding
   "Y" #'(lambda () (interactive)
           (dired-copy-filename-as-kill 0)))
  :init
  (with-eval-after-load 'evil
    (evil-ex-define-cmd "Ex[plore]" 'dired-jump)
    (evil-ex-define-cmd "Sex[plore]" #'(lambda () (interactive)
                                         (call-interactively 'evil-window-split)
                                         (dired-jump)))
    (evil-ex-define-cmd "Vex[plore]" #'(lambda () (interactive)
                                         (call-interactively 'evil-window-vsplit)
                                         (dired-jump)))
    (evil-ex-define-cmd "Tex[plore]" #'(lambda () (interactive)
                                         (if (>= emacs-major-version 27)
                                             (tab-bar-new-tab)
                                           (my-evil-new-tab nil))
                                         (dired-jump)))))

(provide 'config-emacs)

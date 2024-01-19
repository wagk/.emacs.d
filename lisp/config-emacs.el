(require 'use-package)
(require 'config-evil)

;; Catch-all config for emac variables
(use-package emacs
  :demand t
  :ensure nil
  :straight nil
  :custom
  (fill-column 80)
  (ff-always-try-to-create nil)
  :config
  (cl-defun --point-to-file-and-line-number ()
    (interactive)
    (require 'project)
    (let* ((buf (if (project-current nil)
                  (file-relative-name (buffer-file-name)
                                      (project-root (project-current)))
                  (buffer-file-name)))
           (info (concat buf
                         ":"
                         (number-to-string (line-number-at-pos)))))
      (kill-new info)
      (message "%s" info)))

  (cl-defun --kill-buffer-path ()
    (interactive)
    (let ((name (or (buffer-file-name) dired-directory)))
      (pcase name
        ;; hack since dired-directory might be a list
        ((or 'nil (pred listp)) (message "Not a file"))
        (name (kill-new name)
              (message "%s" name)))))

  (evil-ex-define-cmd "byl" #'--point-to-file-and-line-number)
  (evil-ex-define-cmd "byf" #'--kill-buffer-path))

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
  (dired-mode-map
   :states 'normal
   :prefix my-default-evil-leader-key
   "<SPC>" 'execute-extended-command)
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

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items nil)
  :config
  (recentf-mode)
  (with-eval-after-load 'consult
    (evil-ex-define-cmd "recent" 'consult-recent-file)))

(provide 'config-emacs)

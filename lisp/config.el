(setq user-full-name    "Pang Tun Jiang"
      user-mail-address "mail@wagk.org")

;; buffer encoding systems
;; We do this here because the package system might need to know our preferences
(customize-set-variable        'locale-coding-system 'utf-8)
(prefer-coding-system          'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-charset-priority          'unicode)
(set-clipboard-coding-system   'utf-8)
(set-default-coding-systems    'utf-8)
(set-file-name-coding-system   'utf-8)
(set-keyboard-coding-system    'utf-8)
(set-selection-coding-system   'utf-8)
(set-terminal-coding-system    'utf-8)

(defconst user-init-file
  (locate-user-emacs-file "init.el")
  "Points to init.el.")

(defconst user-local-file
  (locate-user-emacs-file "local.el")
  "Points to local.el.")

(defconst user-lisp-dir
  (locate-user-emacs-file "lisp")
  "Points to LISP configuration file directory.")

(defun find-user-init-file ()
  "Edit `user-init-file' without opening a new window."
  (interactive)
  (find-file user-init-file))

(defun find-user-local-file ()
  "Edit `local.el' without opening a new window."
  (interactive)
  (find-file user-local-file))

(defun find-user-lisp-dir ()
  "Edit LISP directory without opening a new window."
  (interactive)
  (dired user-lisp-dir))

(defgroup personal nil
  "A list of personal configuration variables.")

(setq initial-scratch-message "")

;; (setq initial-scratch-message
;;       (concat "> Programmers are not to be measured by their ingenuity and their\n"
;;               "> logic but by the completeness of their case analysis.\n"
;;               "\n"))

;; Emacs considers the following "dangerous" (i.e they'll ask you to
;; confirm)
(put 'list-timers 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Load local configuration variables. It should not assume any
;; packages are loaded.
(when (file-exists-p user-local-file)
  (load-file user-local-file))

(require 'config-prelude)
(require 'config-theme)
(require 'config-helpers)
(require 'config-evil)
(require 'config-git)
(require 'config-completions)
(require 'config-project)
(require 'config-emacs)
(require 'config-text)
(require 'config-qol)
;; (require 'config-lsp)
(require 'config-org)
(require 'config-org-capture)
(require 'config-language)
(require 'config-todo)

(require 'config-japanese)
(require 'config-anki)
(require 'config-markdown)

(provide 'config)

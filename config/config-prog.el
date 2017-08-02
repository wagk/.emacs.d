;;; config-prog.el --- umbrella program package configuration file

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)

(use-package ctags-update
  :ensure t
  :config
  (autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on 'ctags-auto-update-mode'." t))

(evil-declare-key 'insert 'prog-mode-map
  (kbd "RET") 'comment-indent-new-line)

;; make _ be recognised as part of a word
(defun /treat-underscore-as-word ()
  "Make underscore be considered part of a word, just like vim"
  (modify-syntax-entry ?_ "w"))

(add-hook 'prog-mode-hook #'/treat-underscore-as-word)

(provide 'config-prog)

;;; config-prog.el ends here

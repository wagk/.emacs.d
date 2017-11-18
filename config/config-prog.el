;;; config-prog.el --- umbrella program package configuration file

;;; Commentary:

;;; Code:
(require 'config-package)
(require 'config-evil)

(use-package ctags-update
  :init
  (autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on 'ctags-auto-update-mode'." t))

(evil-declare-key 'insert 'prog-mode-map
  (kbd "RET") 'comment-indent-new-line)

(add-hook 'prog-mode-hook #'/treat-underscore-as-word)

(provide 'config-prog)

;;; config-prog.el ends here

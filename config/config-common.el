;;; config-common.el --- Headers and packages other packages need

;;; Commentary:

;;; Code:
(require 'config-package)

(with-no-warnings
  (require 'cl))

(use-package dash)

(use-package s)

(use-package f)

(use-package misc-cmds
  :disabled t)

(defun my-find-parent-major-modes ()
  "docstring for my-find-parent-major-modes"
  (let ((parents (list major-mode)))
    (while (get (car parents) 'derived-mode-parent)
      (push (get (car parents) 'derived-mode-parent) parents))
    parents))

(provide 'config-common)

;;; config-common.el ends here

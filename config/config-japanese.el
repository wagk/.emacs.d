;;; config-japanese.el --- japanese config

;;; Commentary:
;; TODO: convert this to use-package

;;; Code:
(require 'config-package)

(require 'kkc)
(eval-after-load "kkc"
  (progn
    (setq default-input-method "japanese"
          kkc-show-conversion-list-count 1)
    ;; (define-key kkc-keymap (kbd "SPC")       'kkc-terminate)
    ;; (define-key kkc-keymap (kbd "<tab>")     'kkc-next)
    ;; (define-key kkc-keymap (kbd "<backtab>") 'kkc-prev)
    )
  )

(provide 'config-japanese)

;;; config-japanese.el ends here

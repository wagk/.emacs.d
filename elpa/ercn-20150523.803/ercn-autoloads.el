;;; ercn-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ercn" "ercn.el" (22646 62626 0 0))
;;; Generated autoloads from ercn.el

(autoload 'ercn-match "ercn" "\
Extracts information from the buffer and fires `ercn-notify-hook' if needed.

\(fn)" nil nil)

(autoload 'ercn-fix-hook-order "ercn" "\
Notify before timestamps are added

\(fn &rest _)" nil nil)

(when (boundp 'erc-modules) (ercn--add-erc-module))

(eval-after-load 'erc '(progn (require 'ercn) (ercn--add-erc-module)))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ercn-autoloads.el ends here

;;; evil-extra-operator-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "evil-extra-operator" "evil-extra-operator.el"
;;;;;;  (22682 50519 0 0))
;;; Generated autoloads from evil-extra-operator.el

(autoload 'evil-operator-eval "evil-extra-operator" "\
Evil operator for evaluating code." t)

(autoload 'evil-operator-google-translate "evil-extra-operator" "\
Evil operator for translating text via google translate." t)

(autoload 'evil-operator-google-search "evil-extra-operator" "\
Evil operator for google search." t)

(autoload 'evil-operator-highlight "evil-extra-operator" "\
Evil operator for region highlight." t)

(autoload 'evil-operator-fold "evil-extra-operator" "\
Evil operator for folding region." t)

(autoload 'evil-operator-org-capture "evil-extra-operator" "\
Evil operator for org-capture." t)

(autoload 'evil-operator-remember "evil-extra-operator" "\
Evil operator for remember-region" t)

(autoload 'evil-operator-query-replace "evil-extra-operator" "\
Evil operator to query and replace a region throughout the current buffer" t)

(autoload 'evil-operator-clone "evil-extra-operator" "\
Evil operator to create a clone of a motion" t)

(autoload 'evil-extra-operator-mode "evil-extra-operator" "\
Buffer local minor mode to enable extra operators for Evil.

\(fn &optional ARG)" t nil)

(autoload 'evil-extra-operator-mode-install "evil-extra-operator" "\


\(fn)" nil nil)

(defvar global-evil-extra-operator-mode nil "\
Non-nil if Global Evil-Extra-Operator mode is enabled.
See the `global-evil-extra-operator-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-extra-operator-mode'.")

(custom-autoload 'global-evil-extra-operator-mode "evil-extra-operator" nil)

(autoload 'global-evil-extra-operator-mode "evil-extra-operator" "\
Toggle Evil-Extra-Operator mode in all buffers.
With prefix ARG, enable Global Evil-Extra-Operator mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Evil-Extra-Operator mode is enabled in all buffers where
`evil-extra-operator-mode-install' would do it.
See `evil-extra-operator-mode' for more information on Evil-Extra-Operator mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-extra-operator-autoloads.el ends here

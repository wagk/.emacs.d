;;; evil-avy-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "evil-avy" "evil-avy.el" (22682 50526 0 0))
;;; Generated autoloads from evil-avy.el

(defvar evil-avy-mode nil "\
Non-nil if Evil-Avy mode is enabled.
See the `evil-avy-mode' command
for a description of this minor mode.")

(custom-autoload 'evil-avy-mode "evil-avy" nil)

(autoload 'evil-avy-mode "evil-avy" "\
Toggle evil-avy-mode.
Interactively with no argument, this command toggles the mode. A
positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode,`toggle' toggles the state.

When evil-avy-mode is active, it replaces some the normal, visual, operator
and motion state keybindings to invoke avy commands.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-avy-autoloads.el ends here

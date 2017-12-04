;;; evil-quickscope-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "evil-quickscope" "evil-quickscope.el" (22901
;;;;;;  20893 321565 703000))
;;; Generated autoloads from evil-quickscope.el

(autoload 'evil-quickscope-find-char "evil-quickscope" "\
Move to the next COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word.

\(fn)" t nil)

(autoload 'evil-quickscope-find-char-backward "evil-quickscope" "\
Move to the previous COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word.

\(fn)" t nil)

(autoload 'evil-quickscope-find-char-to "evil-quickscope" "\
Move before the next COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word.

\(fn)" t nil)

(autoload 'evil-quickscope-find-char-to-backward "evil-quickscope" "\
Move before the previous COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word.

\(fn)" t nil)

(autoload 'evil-quickscope-always-mode "evil-quickscope" "\
Quickscope mode for evil. Highlights per-word targets for f,F,t,T vim
movement commands. Target highglights always on.

\(fn &optional ARG)" t nil)

(defvar global-evil-quickscope-always-mode nil "\
Non-nil if Global Evil-Quickscope-Always mode is enabled.
See the `global-evil-quickscope-always-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-quickscope-always-mode'.")

(custom-autoload 'global-evil-quickscope-always-mode "evil-quickscope" nil)

(autoload 'global-evil-quickscope-always-mode "evil-quickscope" "\
Toggle Evil-Quickscope-Always mode in all buffers.
With prefix ARG, enable Global Evil-Quickscope-Always mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Evil-Quickscope-Always mode is enabled in all buffers where
`turn-on-evil-quickscope-always-mode' would do it.
See `evil-quickscope-always-mode' for more information on Evil-Quickscope-Always mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-evil-quickscope-always-mode "evil-quickscope" "\
Enable `evil-quickscope-mode'.

\(fn)" t nil)

(autoload 'turn-off-evil-quickscope-always-mode "evil-quickscope" "\


\(fn)" t nil)

(autoload 'evil-quickscope-mode "evil-quickscope" "\
Quickscope mode for evil. Highlights per-word targets for f,F,t,T vim
movement commands. Target highlights activate when f,F,t,T pressed.

\(fn &optional ARG)" t nil)

(defvar global-evil-quickscope-mode nil "\
Non-nil if Global Evil-Quickscope mode is enabled.
See the `global-evil-quickscope-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-quickscope-mode'.")

(custom-autoload 'global-evil-quickscope-mode "evil-quickscope" nil)

(autoload 'global-evil-quickscope-mode "evil-quickscope" "\
Toggle Evil-Quickscope mode in all buffers.
With prefix ARG, enable Global Evil-Quickscope mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Evil-Quickscope mode is enabled in all buffers where
`turn-on-evil-quickscope-mode' would do it.
See `evil-quickscope-mode' for more information on Evil-Quickscope mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-evil-quickscope-mode "evil-quickscope" "\
Enable `evil-quickscope-mode'.

\(fn)" t nil)

(autoload 'turn-off-evil-quickscope-mode "evil-quickscope" "\
Disable `evil-quickscope-mode'.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-quickscope-autoloads.el ends here

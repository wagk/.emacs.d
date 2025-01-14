fundamental-mode

(/date (format-time-string "%Y-%m-%d"))

prog-mode

(c (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) q)
(dd (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "DEBUG (pangt): " q)
(ff (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME (pangt): " q)
(nn (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "NOTE (pangt): " q)
(tt (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO (pangt): " q)

rust-mode rust-ts-mode

(l "|" p "|" q)

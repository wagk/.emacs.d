;; look up `tempo-define-template`

fundamental-mode

(/date (format-time-string "%F"))
(/now (format-time-string "%F %H:%M"))

prog-mode

(c (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) q)
(dd (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "DEBUG (pangt): " q)
(ff (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME (pangt): " q)
(nn (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "NOTE (pangt): " q)
(tt (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO (pangt): " q)

rust-mode rust-ts-mode

(M "=> " q)
(m "-> " q)

;; lambda expansion
(l "|" p "|" q)

;; docstrings
(d "/// " q)
(D "//! " q)

;; asserts
(a  "assert!(" p ");" q)
(an "assert_ne!(" p "," p ");" q)
(ae "assert_eq!(" p "," p ");" q)

(ma "matches!(" p "," (s pattern) ");" q)

(f "fn " (p "foo")  "(" p ")" p "{"
   n> (p "todo!()") q
   n "}" >)
(pf "pub fn " (p "foo")  "(" p ")" p "{"
    n> (p "todo!()") q
    n "}" >)
(af "async fn " (p "foo")  "(" p ")" p "{"
    n> (p "todo!()") q
    n "}" >)
(paf "pub async fn " (p "foo")  "(" p ")" p "{"
     n> (p "todo!()") q
     n "}" >)

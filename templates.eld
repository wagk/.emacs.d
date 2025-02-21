;; All the Tempo syntax elements are fully supported. The syntax elements
;; are described in detail in the docstring of tempo-define-template in
;; tempo.el. We document the important ones here:
;;
;; “string” Inserts a string literal.
;; p Inserts an unnamed placeholder field.
;; n Inserts a newline.
;; > Indents with indent-according-to-mode.
;; r Inserts the current region. If no region is active, quits the containing template when jumped to.
;; r> Acts like r, but indent region.
;; n> Inserts a newline and indents.
;; & Insert newline unless there is only whitespace between line start and point.
;; % Insert newline unless there is only whitespace between point and line end.
;; o Like % but leaves the point before newline.
;; (s NAME) Inserts a named field.
;; (p PROMPT <NAME> <NOINSERT>) Insert an optionally named field with a prompt. The PROMPT is displayed directly in the buffer as default value. If NOINSERT is non-nil, no field is inserted. Then the minibuffer is used for prompting and the value is bound to NAME.
;; (r PROMPT <NAME> <NOINSERT>) Insert region or act like (p ...).
;; (r> PROMPT <NAME> <NOINSERT>) Act like (r ...), but indent region.
;; Furthermore Tempel supports syntax extensions:
;;
;; (p FORM <NAME> <NOINSERT>) Like p described above, but FORM is evaluated.
;; (FORM ...) Other Lisp forms are evaluated. Named fields are lexically bound.
;; q Quits the containing template when jumped to.

fundamental-mode text-mode prog-mode

(/date (format-time-string "%F"))
(/now (format-time-string "%F %H:%M"))

prog-mode

(c (--tempel-maybe-comment) q)
(dd (--tempel-maybe-comment) "DEBUG (pangt): " q)
(ff (--tempel-maybe-comment) "FIXME (pangt): " q)
(nn (--tempel-maybe-comment) "NOTE (pangt): " q)
(tt (--tempel-maybe-comment) "TODO (pangt): " q)

emacs-lisp-mode

(l "(lambda (" p ")" p ")" q >)

markdown-mode gfm-mode org-mode

(-- "- [ ] " q)

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

(matches "matches!(" p "," (s pattern) ")" q)

(print "// DEBUG (pangt): " > n>
       "println!(\"" q "\");" >)

(dbg "// DEBUG (pangt): " > n>
     "dbg!(" q ");" >)

(i "impl " p " {" > n> q > n "}" >)

;; functions
(f "fn " (p "foo")  "(" p ") " p "{"
   n> (p "todo!()") q >
   n "}" >)

(pf "pub fn " (p "foo")  "(" p ") " p "{"
    n> (p "todo!()") q >
    n "}" >)

(af "async fn " (p "foo")  "(" p ") " p "{"
    n> (p "todo!()") q >
    n "}" >)

(paf "pub async fn " (p "foo")  "(" p ") " p "{"
     n> (p "todo!()") q >
     n "}" >)

;; tests
(t "#[test]" >
   n "fn " (p "foo") "() {" >
   n> (p "todo!()") q >
   n "}" >)

(at "#[tokio::test]" >
   n "async fn " (p "foo") "() {" >
   n> (p "todo!()") q >
   n "}" >)

(tm "#[cfg(test)]" >
    n "mod tests {" >
    n> "use super::*;" >
    n>
    n> q >
    n "}" >)

;; structs/enums

(ps "pub struct " q >)
(s "struct " q >)

(pe "pub enum " (s "Foo") " {" n> p n "}" q >)
(e "enum " (s "Foo") " {" n> p n "}" q >)

org-mode

(src "#+BEGIN_SRC " > q n>
     "#+END_SRC")

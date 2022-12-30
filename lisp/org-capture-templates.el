(require 'config::org
         (locate-user-emacs-file "lisp/org.el"))
(require 'config::variables
         (locate-user-emacs-file "variables.el")) ;; --todo-file, --done-file

(require 'evil)
(require 'org-capture)

(when (and (bound-and-true-p --done-file) (f-exists-p --done-file))
  (my-evil-define-split-vsplit-cmd "done[file]" #'(lambda () (interactive) (find-file --done-file)))
  (setq org-capture-templates
        (doct-add-to
         org-capture-templates
         '(("Add to done.txt"
            :keys "done"
            :empty-lines 0
            :file --done-file
            :type plain
            :immediate-finish t
            :before-finalize markdown-table-align
            :template "| %<%F> | %^{tags} | %^{thing} |"
            :function (lambda ()
                        (end-of-buffer)
                        (insert ?\n)
                        (end-of-buffer)))))))

(when (and (bound-and-true-p --todo-file) (f-exists-p --todo-file))
  (my-evil-define-split-vsplit-cmd "todo[file]" #'(lambda () (interactive) (find-file --todo-file)))
  (setq org-capture-templates
        (doct-add-to
         org-capture-templates
         '(("Add to todo.txt"
            :keys "todo"
            :empty-lines 0
            :file --todo-file
            :type plain
            :immediate-finish t
            :before-finalize markdown-table-align
            :template "| %<%F> | %^{tags} | %^{thing} |"
            :function (lambda ()
                        (end-of-buffer)
                        (insert ?\n)
                        (end-of-buffer)))))))

(with-eval-after-load 'deft
  (setq org-capture-templates
        (doct-add-to
         org-capture-templates
         '(("Add to today's diary"
            :keys "diary"
            :empty-lines 0
            :type plain
            :file (lambda () (f-join deft-directory (format-time-string "%F.md")))
            :template ("# %<%F>" "%?"))))))

(provide 'config::org-capture-templates)

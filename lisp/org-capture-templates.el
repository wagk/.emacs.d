(require 'config::org
         (locate-user-emacs-file "lisp/org.el"))
(require 'config::variables
         (locate-user-emacs-file "variables.el")) ;; --todo-file, --done-file

(require 'evil)
(require 'org-capture)

(when (not (bound-and-true-p --diary-file))
  (warn "variable `--diary-file' not set!"))

(with-eval-after-load 'doct
  (setq org-capture-templates
        (doct-add-to
          org-capture-templates
          '(("Add to today's diary"
             :keys "today"
             :empty-lines 1
             :file --diary-file
             :template "* %{todo-state} %? %^g"
             :datetree t
             :children
             (("done"
               :keys "done"
               :todo-state "DONE")
              ("todo"
               :keys "todo"
               :todo-state "TODO")
              ("diary"
               :keys "diary"
               :template "* %?"
               :todo-state nil)))))))

(defun --capture-diary-entry ()
  (interactive)
  (require 'org-capture)
  (org-capture nil "todaydiary"))

(--evil-ex-define-cmds-splits-and-tabs "diary"
                       #'(lambda () (interactive)
                           (find-file --diary-file))
				       --diary-file)

(evil-ex-define-cmd "dd" #'--capture-diary-entry)

(provide 'config::org-capture-templates)

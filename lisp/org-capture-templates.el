(require 'config::org
         (locate-user-emacs-file "lisp/org.el"))
(require 'config::variables
         (locate-user-emacs-file "variables.el")) ;; --todo-file, --done-file

(require 'evil)
(require 'org-capture)

(when (and (bound-and-true-p --done-file) (f-exists-p --done-file))
  (message "Configuring donefile")
  (my-evil-define-split-vsplit-cmd "done[file]" #'(lambda () (interactive) (find-file --done-file)))
  (with-eval-after-load 'doct
    (setq org-capture-templates
          (doct-add-to
           org-capture-templates
           '(("Add to done.txt"
              :keys "done"
              :empty-lines 0
              :file --done-file
              :type plain
              :immediate-finish t
              :before-finalize (lambda ()
                                 (require 'markdown-mode)
                                 (markdown-table-align))
              :template "| %<%F> | %^{tags} | %^{thing} |"
              :function (lambda ()
                          (end-of-buffer)
                          (insert ?\n)
                          (end-of-buffer))))))))

(when (and (bound-and-true-p --todo-file) (f-exists-p --todo-file))
  (message "Configuring todofile")
  (my-evil-define-split-vsplit-cmd "todo[file]" #'(lambda () (interactive) (find-file --todo-file)))
  (with-eval-after-load 'doct
    (setq org-capture-templates
          (doct-add-to
           org-capture-templates
           '(("Add to todo.txt"
              :keys "todo"
              :empty-lines 0
              :file --todo-file
              :type plain
              :immediate-finish t
              :before-finalize (lambda ()
                                 (require 'markdown-mode)
                                 (markdown-table-align))
              :template "| %<%F> | %^{tags} | %^{thing} |"
              :function (lambda ()
                          (end-of-buffer)
                          (insert ?\n)
                          (end-of-buffer))))))))

(with-eval-after-load 'doct
  (when (not (bound-and-true-p --notes-folder))
    (warn "variable --notes-folder not set!"))
  (setq org-capture-templates
        (doct-add-to
          org-capture-templates
          '(("Add to today's diary"
             :keys "diary"
             :empty-lines 0
             :file --diary-file
             :datetree t)))))
(defun --capture-diary-entry ()
  (interactive)
  (require 'org-capture)
  (org-capture nil "diary"))
(evil-ex-define-cmd "diary" #'(lambda ()
                                (interactive)
                                (find-file --diary-file)))
(evil-ex-define-cmd "dd" #'--capture-diary-entry)

(provide 'config::org-capture-templates)

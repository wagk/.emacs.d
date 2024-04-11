(require 'config-org)

(use-package anaphora)

(use-package org-capture
  :ensure nil ;; because org-capture is from org
  :after (org)
  :defer 20
  :commands (org-capture
             org-capture-templates)
  :general
  (org-capture-mode-map
   [remap evil-save-and-close]          'org-capture-finalize
   [remap evil-save-modified-and-close] 'org-capture-finalize
   [remap evil-quit]                    'org-capture-kill)
  :hook ((org-capture-mode-hook . evil-insert-state)
         (org-capture-after-finalize-hook . org-align-all-tags))
  :custom
  (org-capture-bookmark nil)
  :init
  (defun --org-capture-completing-read ()
    "Select capture template, stolen from
   https://github.com/raxod502/selectrum/wiki/Useful-Commands#user-content-org-capture-template"
    (interactive)
    (require 'org-capture)
    (require 'anaphora)
    (let (prefixes)
      (alet (mapcan (lambda (x)
                      (let ((x-keys (car x)))
                        ;; Remove prefixed keys until we get one that matches the current item.
                        (while (and prefixes
                                    (let ((p1-keys (caar prefixes)))
                                      (or (<= (length x-keys) (length p1-keys))
                                          (not (string-prefix-p p1-keys x-keys)))))
                          (pop prefixes))
                        (if (> (length x) 2)
                            (let ((desc (mapconcat #'cadr (reverse (cons x prefixes)) " | ")))
                              (list (format "%-5s %s" x-keys desc)))
                          (push x prefixes)
                          nil)))
                    (-> org-capture-templates
                        (org-capture-upgrade-templates)
                        (org-contextualize-keys org-capture-templates-contexts)))
        (funcall #'org-capture nil (car (split-string (--completing-read "Capture template: " it
                                                                         :require-match t)))))))
  (evil-ex-define-cmd "cc" '--org-capture-completing-read)
  :config
  (defun --select-org-agenda-file ()
    (require 'org-agenda)
    (let ((targets
           (->> org-agenda-files
                (-map-when
                 #'f-directory-p
                 #'(lambda (dir)
                     (f-files dir
                              (lambda (file)
                                (string-equal (f-ext file) "org")))))
                (-flatten))))
      (if (eq 1 (safe-length targets))
          (car targets)
        (--completing-read "Capture target: " targets
                           :require-match t))))
  (defun --select-project-todo-file ()
    (require 'org-agenda)
    (require 'project)
    (if-let* ((project (project-current))
              (root (project-root project))
              (todo-file (f-join root "todo.org")))
        (progn
          (unless (f-file-p todo-file)
            (f-touch todo-file))
          todo-file)
      (error "Not in a project directory (%s)" default-directory))))

;; https://github.com/progfolio/doct
(use-package doct
  :ensure (:host github :repo "progfolio/doct"))

(with-eval-after-load 'org-capture
  (require 'doct)
  (cl-defun --scratch-capture-template ()
    (let* ((line-number (number-to-string (line-number-at-pos)))
           (filepath (pcase-exhaustive (list (project-current nil)
                                             (buffer-file-name))
                       (`(nil nil) "<unbacked buffer>")
                       (`(,proj nil) "<unbacked project buffer>")
                       (`(nil ,name) (file-name-nondirectory name))
                       (`(,proj ,name) (file-relative-name
                                        name (project-root proj))))))
      (concat filepath ":" line-number " %?"
              (when (use-region-p)
                (concat "\n\n"
                        "```\n"
                        "%i" ;; the region itself would likely have a newline
                        "```")))))
  (setq org-capture-templates
        (doct-add-to
         org-capture-templates
         `(("Scratch"
            :keys "scratch"
            :type plain
            :function
            ,#'(lambda () (scratch-buffer) (goto-char (point-max)))
            :empty-lines 1
            :unnarrowed nil
            :no-save t
            :template
            ,#'--scratch-capture-template
            :after-finalize
            ,#'(lambda () (setq org-capture-last-stored-marker (make-marker))))))))

(with-eval-after-load 'evil
  (evil-ex-define-cmd "ss" #'(lambda () (interactive)
                               (require 'org-capture)
                               (org-capture nil "scratch"))))

(provide 'config-org-capture)

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
  (cl-defun --org-capture-completing-read ()
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
  (cl-defun --select-org-agenda-file ()
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
  (cl-defun --select-project-todo-file ()
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
  :after org-capture
  :ensure (:host github :repo "progfolio/doct"))

(cl-defun --dedent-text (text)
  (with-temp-buffer
    (insert text)
    (untabify (point-min) (point-max))
    (let ((dedent-by (indent-rigidly--current-indentation (point-min) (point-max))))
      (indent-rigidly (point-min) (point-max) (- dedent-by))
      (buffer-substring (point-min) (point-max)))))

(cl-defun --capture-template-interesting ()
  "Populates an `org-capture' template that stores interesting information.
Note that capture templates are called in the originating buffer; the one you
were at when you called for the capture.

Assumes Markdown formatting."
  (let* ((region (when (use-region-p)
                   (let* ((beg (region-beginning))
                          (end (region-end))
                          (text (buffer-substring beg end)))
                     (--dedent-text text))))
         (filepath (pcase-exhaustive (list (project-current nil)
                                           (buffer-file-name))
                     (`(,_ nil) (buffer-name))
                     (`(nil ,name) (file-name-nondirectory name))
                     (`(,proj ,name) (file-relative-name
                                      name (project-root proj)))))
         (line-number (number-to-string (line-number-at-pos)))
         (filepath-and-line (concat filepath ":" line-number)))
    ;; (concat (format "## %s %%^{What's interesting?}\n" (format-time-string "%F"))
    (concat (format "%s %%?\n" (format-time-string "%F %H:%M")
                    (when region (concat (format "\nAt `%s`:\n" filepath-and-line
                                                "```\n"
                                                region)
                                         "```"))))))

(cl-defun --HACK-discard-last-stored-marker ()
  "Org assumes that the capture will be done inside an `org' buffer and
helpfully does some parsing(?) if this marker is set as it should be. Remove
this assumption."
  (setq org-capture-last-stored-marker (make-marker)))

(with-eval-after-load 'org-capture
  (require 'doct)
  (setq org-capture-templates
        (doct-add-to
         org-capture-templates
         `(("Scratch"
            :keys "scratch"
            :type plain
            :function
            ,#'(lambda () (scratch-buffer) (goto-char (point-max)))
            :empty-lines-before 1
            :unnarrowed nil
            :no-save t
            :template
            ,#'--capture-template-interesting
            :after-finalize
            ,#'--HACK-discard-last-stored-marker)))))

(with-eval-after-load 'evil
  (evil-ex-define-cmd "ss" #'(lambda () (interactive)
                               (require 'org-capture)
                               (org-capture nil "scratch"))))

(provide 'config-org-capture)

(use-package project
  :ensure nil
  :init
  (advice-add 'evil-make :before
              #'(lambda (&rest _) (require 'project)))
  :config
  ;; NOTE: no idea why this here is necessary; it gets overwritten otherwise
  (general-define-key
   :keymaps 'project-prefix-map
   "d" 'project-find-dir
   "D" 'project-dired
   "t" '(lambda ()
          (interactive)
          (let ((default-directory (project-root (project-current t))))
            (call-interactively 'gdb))))

  (with-eval-after-load 'compile
    (cl-defun --project-buffer-name-function (mode-name)
      (require 'project)
      (concat "*" (downcase mode-name)
              (if-let* ((project (project-current nil)))
                  (concat "<" (project-name project) ">"))
              "*"))
    (customize-set-variable 'compilation-buffer-name-function
                            #'--project-buffer-name-function))

  (with-eval-after-load 'consult
    (general-define-key
     :keymaps 'project-prefix-map
     "b" 'consult-project-buffer
     "C-c" 'consult-compile-error))

  (with-eval-after-load 'evil
    (cl-defun --project-find-dir-in-split ()
      (interactive)
      (--evil-do-in-split #'project-find-dir :split))
    (cl-defun --project-find-dir-in-vsplit ()
      (interactive)
      (--evil-do-in-split #'project-find-dir :vsplit))

    (cl-defun --project-find-file-in-split ()
      (interactive)
      (--evil-do-in-split #'project-find-file :split))
    (cl-defun --project-find-file-in-vsplit ()
      (interactive)
      (--evil-do-in-split #'project-find-file :vsplit))

    (cl-defun --project-dired-in-split ()
      (interactive)
      (--evil-do-in-split #'project-dired :split))
    (cl-defun --project-dired-in-vsplit ()
      (interactive)
      (--evil-do-in-split #'project-dired :vsplit))

    (general-define-key
     :keymaps 'evil-window-map
      "D v"     #'--project-dired-in-vsplit
      "C-D C-v" #'--project-dired-in-vsplit
      "D s"     #'--project-dired-in-split
      "C-D C-s" #'--project-dired-in-split
      "d v"     #'--project-find-dir-in-vsplit
      "C-d C-v" #'--project-find-dir-in-vsplit
      "d s"     #'--project-find-dir-in-split
      "C-d C-s" #'--project-find-dir-in-split))

  (with-eval-after-load 'evil
    (cl-defun --project-any-command-in-split ()
      (interactive)
      (--evil-do-in-split #'project-any-command :split))
    (cl-defun --project-any-command-in-vsplit ()
      (interactive)
      (--evil-do-in-split #'project-any-command :vsplit))
    (general-define-key
     :keymaps 'evil-window-map
      "V"   #'--project-any-command-in-vsplit
      "C-V" #'--project-any-command-in-vsplit
      "S"   #'--project-any-command-in-split
      "C-S" #'--project-any-command-in-split))

  (with-eval-after-load 'multi-term
    (general-define-key
     :keymaps 'project-prefix-map
     "s" #'(lambda ()
             (interactive)
             (if (file-remote-p (buffer-file-name))
                 (project-shell)
               (multi-term)))))

;; (cl-defun --compile-command-completing-read ()
;;   (interactive)
;;   (require 'dash)
;;   (require 'project)
;;   (require 'savehist)
;;   (let* ((name (project-name (project-current)))
;;          (history-name (intern (concat "compile-history-" name))))
;;     ;; If variable doesn't exist yet, create it and mark it for persistence.
;;     (unless (boundp history-name)
;;       (set history-name (list)))
;;     (add-to-list 'savehist-additional-variables history-name)
;;     (let ((command (--completing-read "Compile command: "
;;                                       (eval history-name)))
;;           (default-directory (or (project-root (project-current))
;;                                  default-directory)))
;;       (add-to-history history-name command)
;;       (compile command))))

  (cl-defun --compile-command-completing-read ()
    (interactive)
    (require 'project)
    (let ((command (--completing-read "Compile command: "
                                      compile-history))
          (default-directory (or (project-root (project-current))
                                 default-directory)))
      (add-to-history 'compile-history command)
      (compile command)))

  (defun --compile-command-delete ()
    "Finds a compile command and removes it from `compile-history'"
    (interactive)
    (require 'dash)
    (require 'project)
    (let ((command (--completing-read "Delete compile command: "
                                      compile-history
                                      :require-match t
                                      :default-value compile-command)))
      (when (y-or-n-p (format "Delete command \"%s\"?" command))
        (setq compile-history (remove command compile-history)))))

  (general-define-key
   :keymaps 'project-prefix-map
   "c" #'--compile-command-completing-read)

  (defun --project-find-readme ()
    "Finds and lists all the README (eventually documentation) files "
    (interactive)
    (require 'dash)
    (let* ((case-fold-search t) ; ignore cases
           ;; same reasoning as `--compile-command-completing-read'
           (project-dir (project-root (project-current)))
           (readme-list (->> (project-files (project-current))
                             (-keep (lambda (elem)
                                      (when (string-match-p "readme" elem nil)
                                        elem)))
                             (mapcar #'(lambda (file)
                                         (f-relative file
                                          project-dir)))))
           (readme (if (eq 1 (length readme-list))
                      (car readme-list)
                    (--completing-read "README: " readme-list
                                       :require-match t
                                       :default-value "README.md")))
           (readme-path (f-join project-dir readme)))
      (unless (f-exists-p readme-path)
        (f-touch readme-path))
      (find-file readme-path)))

  (with-eval-after-load 'evil
    (evil-set-command-property #'project-find-file :jump t)
    (evil-set-command-property #'project-dired :jump t)
    (evil-set-command-property #'project-find-dir :jump t)
    (evil-ex-define-cmd "pss" #'project-async-shell-command))

  (with-eval-after-load 'config-evil-helpers
    (--evil-define-splits "readme" #'--project-find-readme)
    (--evil-define-splits "pff" #'project-find-file)
    (--evil-define-splits "pd" #'project-dired)
    (--evil-define-splits "pdd" #'project-find-dir)
    (--evil-define-splits "pp" #'(lambda () (interactive)
                                   (dired (funcall project-prompter))))
    (if (eq system-type 'windows-nt)
        (evil-ex-define-cmd "ps" #'project-shell)
      (--evil-define-splits "ps" #'--multi-vterm-project))
    (evil-ex-define-cmd "pc" #'--compile-command-completing-read)
    (evil-ex-define-cmd "pcd" #'--compile-command-delete)
    (evil-ex-define-cmd "pck" #'kill-compilation)
    (--evil-define-splits "pcb"
                          #'(lambda () (interactive)
                              (let ((buf (cl-find-if
                                          #'compilation-buffer-p
                                          (project-buffers (project-current)))))
                                (if buf
                                    (project-switch-to-buffer buf)
                                  (message "No project compilation buffer detected.")))))
    (evil-ex-define-cmd "pcc" #'(lambda ()
                                  (interactive)
                                  (let ((default-directory
                                          (project-root (project-current))))
                                    (recompile))))

    (with-eval-after-load 'consult
      (evil-set-command-property #'consult-project-buffer :jump t)
      (--evil-define-splits "pbb" #'consult-project-buffer)
      (evil-ex-define-cmd "pr" #'consult-ripgrep))

    (with-eval-after-load 'consult-todo
      (--evil-define-splits "pt" #'--consult-todo))

    (with-eval-after-load 'flycheck
      (evil-ex-define-cmd "pe" #'flycheck-list-errors)))

  (with-eval-after-load 'rg
    (with-eval-after-load 'evil
      (evil-ex-define-cmd "rp" 'rg-project))

    (general-define-key
     :keymaps 'project-prefix-map
     "g" 'rg-project
     "G" 'rg-project)))

;; (with-eval-after-load 'transient
;;   (transient-define-prefix --my-project-hotkeys ()
;;     "Project related convenience bindings."
;;     [:description (lambda ()
;;                     (format "Project: %s\n"
;;                             (project-name (project-current))))
;;      ["Find"
;;       ("f f" "Find file" project-find-file)
;;       ("f d" "Directory" project-find-dir)]])
;;   (with-eval-after-load 'evil
;;     (evil-ex-define-cmd "pp" #'--my-project-hotkeys)))

(defconst --jira-regex (rx (one-or-more upper) "-" (one-or-more digit)))

(cl-defun --find-jira-ticket-in-line ()
  "Find all JIRA IDs in the line."
  (require 'thingatpt)
  (let ((jira-regex --jira-regex)
        (matches '()))
    (save-excursion
      (beginning-of-line)
      (while (re-search-forward jira-regex (line-end-position) t)
        (add-to-list 'matches (substring-no-properties (match-string 0))
                     :append)))
    (cond
     ((length= matches 0) nil)
     ((length= matches 1) (car matches))
     (t (let ((orderless-matching-styles '(orderless-literal)))
          (--completing-read "Jira ID: " matches
                           :require-match t))))))

(provide 'config-project)

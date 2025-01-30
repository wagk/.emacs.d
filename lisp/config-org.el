(require 'use-package)
(require 'config-evil) ;; evil-org

;;; org-mode

(use-package org
  :ensure (:host github :repo "emacs-straight/org-mode" :branch "main")
  :after config-theme
  :commands (orgtbl-mode
             org-babel-load-file)
  :general
  (:states  '(normal motion)
   :keymaps 'org-mode-map
   "TAB"    'org-cycle
   "<tab>"  'org-cycle
   "g x"    'org-open-at-point
   "C-c C-w" 'org-refile)
  (:states 'normal
   :prefix my-default-evil-leader-key
   "o t" 'org-time-stamp
   "o T" #'(lambda () (interactive)
             (org-time-stamp '(16))))
  (org-mode-map
   "C-c C-'" 'org-edit-special
   "<C-M-return>" 'org-insert-subheading
   "<C-M-S-return>" 'org-insert-todo-subheading)
  (org-src-mode-map
   "C-c C-'" 'org-src-edit-exit)
  :custom
  (org-edit-src-content-indentation 0 "Don't indent code blocks")
  (org-list-description-max-indent
   5 "Ideally we should familiarize ourselves with adding a newline
   after each list description entry, like so:
   - Description ::
         Text describing description
   Currently it's:
   - Description :: Text describing description
   And this style breaks once indentation moves past this custom
   variable")
  (org-support-shift-select
   t "Let me use J in org-mode please.")
  ;; (org-startup-indented t)
  (org-startup-indented nil "Should get used to as little syntax sugar as possible")
  (org-footnote-auto-label 'confirm)
  (org-footnote-section nil)
  (org-indent-mode-turns-on-hiding-stars t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup
   ;; 'current-window
   'split-window-below
   "I tend to have documentation/other things on adjacent windows")
  (org-src-fontify-natively t)
  (org-default-notes-file "~/.todo")
  (org-M-RET-may-split-line nil)
  (org-return-follows-link t)
  (org-todo-keywords '((sequence "TODO(t)" "WAIT(w@)" "|" "DONE(d!/@)" "SKIP(s@/@)")))
  (org-enforce-todo-checkbox-dependencies
   nil "Sometimes we are able to skip dependencies as things happen")
  (org-enforce-todo-dependencies
   nil "Same reason as `org-enforce-todo-checkbox-dependencies'")
  (org-pretty-entities
   ;; nil "It gets a bit annoying when you autocomplete braces")
   t "Try org-appear")
  (org-hide-emphasis-markers t "Try org-appear")
  (org-tags-sort-function 'string-collate-lessp)
  ;; (org-log-done 'time)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-note-clock-out
   ;; t
   nil "I don't think I clock out at good times usually")
  (org-log-redeadline 'time)
  (org-log-reschedule 'time)
  (org-blank-before-new-entry '((heading . t)
                                (plain-list-item . nil)))
  (org-refile-targets '((nil . (:maxlevel . 9))))
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-highlight-latex-and-related '(latex))
  (org-insert-heading-respect-content nil)
  (org-catch-invisible-edits 'smart)
  (org-cycle-separator-lines 0)
  ;; (org-link-descriptive nil "reduce syntax sugar")
  (org-link-descriptive t
                        "Open file links in current window instead of other window")
  (org-adapt-indentation nil "Maintaining indentation for org-files
   looks annoying when editing it as a plain text file")
  ;; (org-list-indent-offset 1)
  (org-extend-today-until
   5 "I think 5 am is a safe bet for the end of the day")
  (org-note-done 'note)
  :custom-face
  (org-meta-line
   ((default . (:extend t
                :inherit (sol-light-foreground
                          sol-superlight-background)))))
  (org-headline-done
   ((default . (:strike-through t))))
  (org-checkbox
   ((default . (:bold t))))
  (org-block
   ((default . (:inherit sol-superlight-background))))
  (org-block-begin-line
   ((default . (:inherit org-meta-line))))
  (org-block-end-line
   ((default . (:inherit org-meta-line))))
  (org-drawer
   ((default . (:inherit sol-light-foreground))))
  (org-special-keyword
   ((default . (:foreground ,sol-blue
                :inherit sol-foreground))))
  :hook ((org-insert-heading-hook . evil-insert-state))
  :init
  (with-eval-after-load 'ol
    (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file))
  (unless (display-graphic-p)
    (general-define-key
     :keymaps 'org-mode-map
     :states '(normal insert motion)
     ;; "C-^" 'org-insert-heading-after-current
     "C-^" 'org-meta-return
     "\236" 'org-insert-todo-heading-respect-content))
  :config
  (defun my-org-convert-list-to-checkbox ()
      (when (and (org-at-item-p)
                (not (org-at-item-checkbox-p)))
        (org-toggle-checkbox '(4)))
    ;; NOTE: for some reason, this hook is not being run
    (add-hook 'org-ctrl-c-ctrl-c-final-hook
              'my-org-convert-list-to-checkbox)
    ;; NOTE: this is a hack, because I've learnt that this hook is not
    ;; consistently being called.
    (advice-add 'org-ctrl-c-ctrl-c :after
                #'(lambda (&rest _)
                    (run-hook-with-args-until-success)
                    'org-ctrl-c-ctrl-c-final-hook)))
  (with-eval-after-load 'evil
    (advice-add 'org-add-note :after #'evil-insert-state)
    ;; NOTE: define our own hacked evil-fill and evil-fill-and-move
    ;; so it will work on list items
    (evil-define-operator my-org-evil-fill (beg end)
      "Fill text."
      :move-point nil
      :type line
      (save-excursion
        (condition-case nil
            (if (org-at-item-p)
                (fill-paragraph nil t)
              (fill-region beg end))
          (error nil))))
    (general-define-key
     :states 'normal
     :keymaps 'org-mode-map
     "gw" 'my-org-evil-fill))
  (with-eval-after-load 'smartparens
    (defun my-dont-close-in-latex-fragment (_open action _context)
      (when (eq action 'insert)
        (org-inside-LaTeX-fragment-p)))
    (sp-local-pair 'org-mode "=" "="
                   :unless '(:add my-dont-close-in-latex-fragment))
    (sp-local-pair 'org-mode "_" "_"
                   :unless '(:add my-dont-close-in-latex-fragment))
    (sp-local-pair 'org-mode "*" "*"
                   :unless '(:add my-dont-close-in-latex-fragment)))
  (customize-set-value 'org-format-latex-options
                       (plist-put org-format-latex-options :scale 1.5))
  (with-eval-after-load 'elec-pair
    (add-hook 'org-mode-hook
              #'(lambda ()
                  (let ((org-pairs '((?= . ?=)
                                     (?* . ?*)
                                     (?$ . ?$))))
                    (setq-local electric-pair-pairs
                                (append electric-pair-pairs org-pairs))
                    (setq-local electric-pair-text-pairs
                                electric-pair-pairs)))))
  (defun my-org-reformat-buffer ()
    (interactive)
    (when (y-or-n-p "Really format current buffer? ")
      (let ((document (org-element-interpret-data (org-element-parse-buffer))))
        (erase-buffer)
        (insert document)
        (goto-char (point-min)))))

  (defun my-org-paste-clipboard-screenshot (&optional dir)
    "Take a screenshot into a time stamped unique-named file in the
     same directory as the org-buffer and insert a link to this file.

Taken from `http://www.sastibe.de/2018/11/take-screenshots-straight-into-org-files-in-emacs-on-win10/`"
    (interactive (list (read-directory-name "" "" "images")))
    (require 'org)
    (unless (equal system-type 'windows-nt)
      (user-error "Implementation currently only works on windows, this is %s"
                  system-type))
    (let ((filename (concat
                     (make-temp-name
                      (concat (file-name-as-directory dir)
                              (-> (buffer-file-name)
                                  file-name-nondirectory
                                  file-name-sans-extension)
                              "_"
                              (format-time-string "%Y-%m-%dT%H%M%S")))
                     ".png")))
      (unless (file-directory-p dir)
        (make-directory dir))
      (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('"
                             filename
                             "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))
      (insert (concat "[[file:" (file-relative-name filename) "]]"))
      (message "Image saved as %s" filename)
      (org-display-inline-images)
      filename))

  (with-eval-after-load 'general
    (general-define-key
      :keymaps 'org-mode-map
      :states '(normal)
      :prefix my-default-evil-leader-key
      "o p" 'my-org-paste-clipboard-screenshot)))

(with-eval-after-load 'org-persist
  (require 'f)
  (customize-set-value 'org-persist-directory
                       (f-join no-littering-var-directory "org-persist/")))

(use-package org-id
  :ensure nil
  :after org
  :defer t
  :custom
  (org-id-ts-format "%s")
  (org-id-method 'ts))

(use-package evil-org
  :ensure (:host github :repo "Somelauw/evil-org-mode")
  :after org
  :blackout t
  :preface
  (fset 'evil-redirect-digit-argument 'ignore)
  :hook ((org-mode-hook . evil-org-mode))
  :custom
  (evil-org-retain-visual-state-on-shift
    t "Let us chain < and > calls")
  (evil-org-use-additional-insert
    t "Add things like M-j to insert")
  (evil-org-special-o/O
    '(table-row) "Do not let o/O affect list items, throws me off")
  (org-special-ctrl-a/e
    t "Pretend leading stars on headlines don't exist when using A/I")
  :general
  (evil-org-mode-map
    :states 'normal
    "g f" 'evil-org-open-links)
  :config
  (when (boundp 'evil-digit-bound-motions)
    (add-to-list 'evil-digit-bound-motions 'evil-org-beginning-of-line))
  (evil-define-key 'motion 'evil-org-mode
      (kbd "0") 'evil-org-beginning-of-line)
  (evil-org-set-key-theme '(textobjects
                            ;; insert ;; replaces c-t and c-d
                            navigation
                            additional
                            shift
                            return
                            operators
                            ;; todo
                            ;; heading
                            calendar)))

(use-package org-web-tools
  :commands
  (org-web-tools-insert-link-for-url
   org-web-tools-insert-web-page-as-entry
   org-web-tools-read-url-as-org
   org-web-tools-convert-links-to-page-entries
   org-web-tools-archive-attach
   org-web-tools-archive-view))

(use-package org-agenda
  :ensure nil
  :after (org config-theme)
  :commands (org-todo-list
             org-agenda-list
             org-agenda-file-to-front
             org-agenda)
  :general
  (:keymaps 'org-agenda-keymap
   :states '(normal motion)
   "g r" 'org-agenda-redo
   "g t" nil
   "g T" nil
   "g l" 'org-agenda-log-mode
   "g x" 'org-open-at-point-global
   ;; Seems to be an unhandled case by evil-org
   "RET" 'org-agenda-switch-to
   [remap evil-write] 'org-save-all-org-buffers
   [remap evil-save-modified-and-close] #'(lambda ()
                                            (interactive)
                                            (org-save-all-org-buffers)
                                            (org-agenda-quit)))
  :custom
  (org-agenda-custom-commands '(("A" "TODOs and Agenda items"
                                 ((alltodo "")
                                  (agenda "")))))
  (org-agenda-start-with-log-mode nil "Use `g l' instead")
  (org-agenda-log-mode-items '(closed clock state))
  (org-agenda-span 'month)
  (org-agenda-window-setup 'current-window)
  (org-agenda-skip-unavailable-files t)
  (org-agenda-time-leading-zero t)
  (org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t%s %?b")
                              (todo . " %i %-12:c%?-12b")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c")))
  (org-agenda-breadcrumbs-separator "/")
  :custom-face
  (org-agenda-done ((t (:inherit org-agenda-done :strike-through t))))
  (org-agenda-date-today ((t (:inherit org-agenda-date :overline t :bold t :inverse t))))
  :hook
  (org-agenda-after-show-hook . org-narrow-to-subtree)
  :init
  (with-eval-after-load 'config-evil-helpers
    (--evil-define-splits "agenda" #'(lambda ()
                                       (interactive (org-agenda nil "A")))))

  (cl-defun --run-with-local-idle-timer (secs repeat function &rest args)
    "Like `run-with-idle-timer', but always runs in the `current-buffer'.
   Cancels itself, if this buffer was killed.
   Stolen from https://emacs.stackexchange.com/a/13275"
   (let* (;; Chicken and egg problem.
          (fns (make-symbol "local-idle-timer"))
          (timer (apply 'run-with-idle-timer secs repeat fns args))
          (fn `(lambda (&rest args)
                 (if (not (buffer-live-p ,(current-buffer)))
                     (cancel-timer ,timer)
                   (with-current-buffer ,(current-buffer)
                    (apply (function ,function) args))))))
     (fset fns fn)
     fn))
  :config
  (require 'evil-org-agenda)
  (customize-set-value 'org-agenda-start-day "-1d"
                       "For some reason this isn't being recognized
                       and loaded in `:custom`")
  (evil-org-agenda-set-keys)
  (with-eval-after-load 'tab-bar
    (general-define-key
     :keymaps 'org-agenda-mode-map
     :states '(motion)
     "g t" 'tab-bar-switch-to-next-tab
     "g T" 'tab-bar-switch-to-prev-tab))

  (define-advice org-agenda-capture (:override () --consult-org-agenda-capture)
    "Overrides `org-agenda-add' with a more consult-like interface"
    (interactive)
    (call-interactively '--org-capture-completing-read))

  ;; override `org-agenda-diary-entry' to use `org-roam'
  (general-define-key
   :keymaps 'org-agenda-mode-map
   :states '(motion)
   "i" #'(lambda () (interactive)
           (require 'org-roam)
           (--org-roam-dailies-today))))

;; https://github.com/alphapapa/org-super-agenda
(use-package org-super-agenda
  :after org-agenda
  :commands org-super-agenda-mode
  :hook (org-agenda-mode-hook . org-super-agenda-mode)
  :general
  (org-super-agenda-mode-map
   "g t" nil
   "g T" nil)
  :custom
  (org-super-agenda-groups
   '((:auto-todo t)))
  ;; '((:name "Blockers"
  ;;    :todo "WAIT")
  ;;   (:name "To Do"
  ;;    :todo "TODO")))
  ;; (:name "Personal Work"
  ;;  :tag "personal"
  ;;  :and (:not (:habit t)))
  ;; (:name "Habits"
  ;;  :habit t)))
  ;; (:name "Unscheduled Work"
  ;;  :not (:scheduled t))))
  :config
  ;; don't let org-super-agenda override evil bindings
  ;; https://github.com/codygman/doom-emacs-literate-config/commit/bcd6ee115db58d12a05ff4aa9ba60f96d87b81ba
  (setq org-super-agenda-header-map (make-sparse-keymap)))

(use-package org-src
  :after org
  :ensure nil
  :init
  (defun my-evil-org-src-save-exit ()
    (interactive)
    (org-edit-src-save)
    (org-edit-src-exit))
  :general
  (org-src-mode-map
   [remap evil-write] 'org-edit-src-save
   ;; doesn't seem to be working, the saving part at least
   [remap evil-save-and-close] #'my-evil-org-src-save-exit
   ;; doesn't seem to be working, the saving part at least
   [remap evil-save-modified-and-close] #'my-evil-org-src-save-exit
   [remap evil-quit] 'org-edit-src-abort))

(use-package ob-async
  :after org)

(use-package ob-http
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (add-to-list 'org-babel-load-languages '(http . t))))

(use-package org-fragtog
  :after org
  :ensure (:host github :repo "io12/org-fragtog")
  :hook (org-mode-hook . org-fragtog-mode))

(use-package org-clock
  :ensure nil
  :after org
  :custom
  (org-clock-clocked-in-display 'both)
  (org-clock-persist 'history)
  (org-clock-mode-line-total 'current)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-history-length 35)
  :general
  (:states '(normal)
   :prefix my-default-evil-leader-key
   "c c" 'my-org-clocking
   "c f" 'my-org-goto-clock)
  :init
  (defun my-org-goto-clock ()
    (interactive)
    (require 'org-clock)
    (org-clock-goto (not (org-clocking-p))))
  (evil-define-command my-org-clocking (&optional bang)
    "Note that adding a ! means you mark the current task as the
  default."
    (interactive "<!>")
    (require 'org-clock)
    (cond
     ((org-clocking-p) (org-clock-out))
     ((and (eq major-mode 'org-mode) (org-at-heading-p))
      (org-clock-in))
     ((string-equal (buffer-name) "*Org Agenda*")
      (org-agenda-clock-in))
     (t (org-clock-in '(4))))
    (when (or bang (not (marker-position org-clock-default-task)))
      (org-clock-mark-default-task)))
  (evil-ex-define-cmd "clock" #'my-org-clocking)
  (evil-ex-define-cmd "clocking" #'my-org-goto-clock)
  :config
  (org-clock-persistence-insinuate)
  (org-clock-load))

(use-package org-appear
  :after org
  :ensure (:host github :repo "awth13/org-appear")
  :commands (org-appear-mode)
  :hook (org-mode-hook . org-appear-mode))

(use-package valign
  :ensure (:host github :repo "emacs-straight/valign" :branch "master")
  :after org
  :hook (org-mode-hook . valign-mode))

;; https://github.com/alphapapa/org-ql
(use-package org-ql
  :after org-super-agenda
  :commands
  (org-ql-search
    org-ql-view
    org-ql-view-sidebar
    org-ql-view-recent-items
    org-ql-sparse-tree))

(provide 'config-org)

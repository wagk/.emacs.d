(require 'use-package)

;;; org-mode

(use-package org
  :straight t
  :demand t
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
  :custom-face
  (org-headline-done ((t (:strike-through t))))
  (org-checkbox ((t (:bold t :box nil))))
  (org-block ((((background dark)) (:background ,sol-base02))
              (((background light)) (:background ,sol-base2))))
  (org-block-begin-line ((((background dark)) (:inherit org-meta-line :underline nil))
                         (((background light)) (:inherit org-meta-line :underline nil))))
  (org-block-end-line ((((background dark)) (:inherit org-meta-line :overline nil))
                       (((background light)) (:inherit org-meta-line :overline nil))))
  (org-drawer ((((background dark)) (:foreground ,sol-base01))
               (((background light)) (:foreground ,sol-base1))))
  (org-special-keyword ((t (:inherit default :bold nil :foreground ,sol-blue))))
  :custom
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
  :hook ((org-insert-heading-hook . evil-insert-state))
  :init
  (with-eval-after-load 'ol
    (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file))
  ;; (customize-set-variable
  ;;  'org-link-frame-setup
  ;;  (let ((alist (copy-alist org-link-frame-setup)))
  ;;    (setf (cdr (assoc 'file alist)) 'find-file)
  ;;    alist)))
  (unless (display-graphic-p)
    (general-define-key
     :keymaps 'org-mode-map
     :states '(normal insert motion)
     ;; "C-^" 'org-insert-heading-after-current
     "C-^" 'org-meta-return
     "\236" 'org-insert-todo-heading-respect-content)))
  ;; (with-eval-after-load 'org
  ;;   (add-hook 'org-mode-hook #'(lambda ()
  ;;                               (with-eval-after-load 'elec-pair
  ;;                                 (let ((org-pairs '((?= . ?=)
  ;;                                                    (?/ . ?/)
  ;;                                                    (?$ . ?$))))
  ;;                                   (setq-local electric-pair-pairs
  ;;                                               (append electric-pair-pairs org-pairs))
  ;;                                   (setq-local electric-pair-text-pairs
  ;;                                               electric-pair-pairs)))))
  ;;   (defun my-org-reformat-buffer ()
  ;;     (interactive)
  ;;     (when (y-or-n-p "Really format current buffer? ")
  ;;       (let ((document (org-element-interpret-data (org-element-parse-buffer))))
  ;;         (erase-buffer)
  ;;         (insert document)
  ;;         (goto-char (point-min)))))
  ;;   (use-package ox-confluence
  ;;     :ensure nil
  ;;     :straight nil
  ;;     :commands org-confluence-export-as-confluence))

  ;; (unless (eq system-type 'windows-nt)
  ;;   (use-package org-contrib
  ;;     :straight t))

(with-eval-after-load 'org-persist
  (customize-set-value 'org-persist-directory (f-join no-littering-var-directory "org-persist/")))

  ;; we do this because juggling between org, org-plus-contrib,
  ;; straight, and emacs' built-in org is horrendous and causing the
  ;; :config code to just not run
(with-eval-after-load 'org
  (defun my-org-convert-list-to-checkbox ()
    (when (and (org-at-item-p)
               (not (org-at-item-checkbox-p)))
      (org-toggle-checkbox '(4))))
  ;; NOTE: for some reason, this hook is not being run
  (add-hook 'org-ctrl-c-ctrl-c-final-hook
            'my-org-convert-list-to-checkbox)
  ;; NOTE: this is a hack, because I've learnt that this hook is not
  ;; consistently being called.
  (advice-add 'org-ctrl-c-ctrl-c :after
              #'(lambda (&rest _)
                  (run-hook-with-args-until-success
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
    ;; NOTE: this is not setting the point properly, as we would expect. so we won't modify this yet
    ;; (evil-define-operator my-org-evil-fill-and-move (beg end)
    ;;   "Fill text and move point to the end of the filled region."
    ;;   :move-point nil
    ;;   :type line
    ;;   (let ((marker (make-marker)))
    ;;     (move-marker marker (1- end))
    ;;     (condition-case nil
    ;;         (progn
    ;;           (if (org-at-item-p)
    ;;               (fill-paragraph nil t)
    ;;             (fill-region beg end))
    ;;           (goto-char marker)
    ;;           (evil-first-non-blank))
    ;;       (error nil))))
    (general-define-key
     :states 'normal
     :keymaps 'org-mode-map
     "gw" 'my-org-evil-fill))
  ;; "gq" 'my-org-evil-fill-and-move))
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
                       (plist-put org-format-latex-options
                                  :scale 1.5))
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

  (use-package ox-confluence
    :ensure nil
    :straight nil
    :commands org-confluence-export-as-confluence))

;; https://github.com/progfolio/doct
(use-package doct
  :straight (:host github :repo "progfolio/doct"))

(provide 'config-org)
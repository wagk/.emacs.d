;;; config-org.el --- org-mode configuration

;;; Commentary:
;; NOTE: Sometimes when calling org-meta-return the cursor
;; positions are all out of whack
;; Answer: fci-mode is fucking things up. Same issue as japanese languge input

;;; Code:

(require 'config-package)
(require 'config-evil)
(require 'config-helm)
(require 'config-buffer)

(use-package org
  :commands
  (org-time-stamp-inactive
   org-refile)

  :general
  (:states 'normal
   :prefix my-default-evil-leader-key
   "o t" 'org-time-stamp-inactive
   "o T" #'my-time-stamp
   "o r" 'org-refile)

  :custom
  (org-support-shift-select t
                            "Let me use J in org-mode please.")
  (org-startup-indented nil)
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-src-tab-acts-natively t)
  ;; (org-src-window-setup 'current-window)
  (org-src-fontify-natively t)
  (org-default-notes-file "~/TODO.org")
  ;; (org-M-RET-may-split-line '((default . nil)))
  (org-M-RET-may-split-line nil)
  (org-enforce-todo-checkbox-dependencies     t)
  (org-enforce-todo-dependencies              t)
  (org-pretty-entities                        nil)
  ;; (org-insert-heading-respect-content t)
  (org-log-done                               'time)
  (org-log-redeadline                         'time)
  (org-log-reschedule                         'time)
  (org-blank-before-new-entry '((heading         . t)
                                (plain-list-item . nil)))
  (org-refile-targets '((nil . (:maxlevel . 9))))
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-highlight-latex-and-related '(latex))

  :config
;;;###autoload
  (defun /org-mode-face-no-resize ()
    "Stop the org-level headers from increasing in height relative to the other
text."
    (when (eq major-mode 'org-mode)
      (dolist (face '(org-level-1
                      org-level-2
                      org-level-3
                      org-level-4
                      org-level-5))
        (set-face-attribute face nil :weight 'semi-bold :height 1.0))))
  (add-hook 'org-mode-hook '/org-mode-face-no-resize)

  ;; (org-toggle-link-display)

  ;; when inserting a heading immediately go into insert mode
  (add-hook 'org-insert-heading-hook 'evil-insert-state)

  ;; (general-define-key :keymaps 'org-mode-map
  ;;                     :states 'insert
  ;;                     "RET"     'newline-and-indent)

  ;; make smartparen autoskip "" because org-mode treats it as a string
  (require 'smartparens)
  (sp-local-pair 'org-mode "\"" nil :when '(:rem sp-in-string-p))

  (defun my-time-stamp ()
    "Prints the time and date."
    (interactive)
    (org-time-stamp-inactive '(16)))

  (require 'evil-embrace)
  (defun my-add-org-evil-embrace-pairs ()
    "Add additional pairings that evil-surround doesn't cover"
    (let ((org-pairs '((?= "=" . "=") ;; verbatim
                       (?* "*" . "*") ;; bold
                       (?_ "_" . "_") ;; underline
                       (?+ "+" . "+") ;; strikethrough
                       (?~ "~" . "~") ;; code
                       (?/ "/" . "/")))) ;; italic
      (dolist (pair org-pairs)
        (embrace-add-pair (car pair) (cadr pair) (cddr pair)))))
  (add-hook 'org-mode-hook 'my-add-org-evil-embrace-pairs)

  (defun my-org-hook-configs ()
    "Hacks to make org-mode less cancer when run"
    ;; NOTE: We turn this off because it is causing the cursor to do really
    ;; fucking weird things
    ;; (require 'fill-column-indicator)
    ;; (turn-on-fci-mode)
    (with-eval-after-load 'display-line-numbers
      (display-line-numbers-mode -1))
    (require 'aggressive-fill-paragraph)
    (aggressive-fill-paragraph-mode))
  (add-hook 'org-mode-hook #'my-org-hook-configs)
  )

;; org capture. https://github.com/syl20bnr/spacemacs/issues/5320
(use-package org-capture
  :ensure nil ;; because org-capture is from org
  :after (org)
  :general
  (:prefix my-default-evil-leader-key
   :states 'normal
   "o o" 'org-capture)
  :config
  (define-key org-capture-mode-map [remap evil-save-and-close]
    'org-capture-finalize)
  (define-key org-capture-mode-map [remap evil-save-modified-and-close]
    'org-capture-finalize)
  (define-key org-capture-mode-map [remap evil-quit]
    'org-capture-kill)
  )

(use-package org-agenda
  :ensure nil ;; because org-agenda is from org
  :after (org)
  :general
  (:prefix my-default-evil-leader-key
   :states 'normal
   "O O" 'org-agenda)
  :config
  ;; initialize org agenda things
  (add-to-list 'org-agenda-files my-org-directory)
  )

;;; This is like a concept map, but in org-files
(use-package org-brain
  :custom
  (org-brain-path my-wiki-directory "Share the same path as deft.")
  (org-brain-file-entries-use-title nil
                                    "Speed optimisation since our filenames and
                                    title should match anyway")
  :general
  (:states 'normal
   :prefix my-default-evil-leader-key
   "N" 'org-brain-visualize)
  :init
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  )

(use-package org-radiobutton)

;; Export orgfiles as anki decks!
;; Looks great for jap study and just study in general
;; https://github.com/louietan/anki-editor
;; Requires that the anki plugin `anki-connect' is installed
(use-package anki-editor)

;; Prepackaged evil bindings for org-mode
;; https://github.com/Somelauw/evil-org-mode
;; Full keybindings:
;; https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org
(use-package evil-org
  ;; :disabled t
  :after (org)
  :demand t
  :diminish (evil-org-mode)
  ;; :general
  ;; (:states '(emacs insert)
  ;;  :keymaps 'org-mode-map
  ;;  "RET" 'evil-org-return)
  :custom
  (evil-org-retain-visual-state-on-shift
   t
   "Let us chain < and > calls")
  (evil-org-use-additional-insert
   t
   "Add things like M-j to insert")
  (evil-org-special-o/O
   '(table-row)
   "Do not let o/O affect list items, throws me off")
  :config
  (evil-org-set-key-theme '(textobjects
                            insert
                            navigation
                            additional
                            shift
                            return
                            operators
                            ;; todo
                            ;; heading
                            calendar
                            ))
  (add-hook 'org-mode-hook 'evil-org-mode)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package worf)

(use-package helm-org-rifle
  :general
  (:states 'normal
   :prefix my-default-evil-leader-key
   "o <SPC>" 'helm-org-rifle)
  :bind
  (:map helm-org-rifle-map
        ("C-w" . evil-delete-backward-word)
        ("\\"  . helm-select-action)
        ("C-j" . helm-next-line)
        ("C-k" . helm-previous-line)
        ("C-n" . helm-next-page)
        ("C-p" . helm-previous-page)
        ("C-l" . helm-next-source)
        ("C-h" . helm-previous-source)
        ("TAB" . helm-execute-persistent-action))
  )

(use-package ob-async
  :demand t
  :after (org))

(use-package ob-clojurescript
  :demand t
  :after (org))

(use-package ob-http
  :demand t
  :after (org))

(use-package ob-browser
  :demand t
  :after (org))

(use-package ob-restclient
  :demand t
  :after (org))

(use-package ob-rust
  :demand t
  :after (org))

(use-package ob-translate
  :demand t
  :after (org))

(provide 'config-org)

;;; config-org ends here

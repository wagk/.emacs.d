;;; init.el --- Bootstrap further configurations

;;; Commentary:
;; Useful resources:
;; https://bling.github.io/blog/2013/10/27/emacs-as-my-leader-vim-survival-guide/
;; https://github.com/bbatsov/emacs-lisp-style-guide
;; https://github.com/noctuid/evil-guide

;; Latest builds can be found at:: alpha.gnu.org/gnu/emacs/pretest/windows/
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/

;;; Code:

;; Note that docstrings for variables come *after* the value

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(defconst my-init-start-time (current-time))

(setq user-full-name    "Pang Tun Jiang"
      user-mail-address "mail@pangt.dev")

;; buffer encoding systems
;; We do this here because the package system might need to know our preferences
(customize-set-variable        'locale-coding-system 'utf-8)
(prefer-coding-system          'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-charset-priority          'unicode)
(set-clipboard-coding-system   'utf-8)
(set-default-coding-systems    'utf-8)
(set-file-name-coding-system   'utf-8)
(set-keyboard-coding-system    'utf-8)
(set-selection-coding-system   'utf-8)
(set-terminal-coding-system    'utf-8)
(if (eq system-type "windows-nt")
    (set-w32-system-coding-system  'utf-8))

(defconst user-init-file
  (locate-user-emacs-file "init.el")
  "Points to init.el.")

(defconst user-config-file
  (locate-user-emacs-file "config.org")
  "Points to config.org.")

(defconst user-local-file
  (locate-user-emacs-file "local.el")
  "Points to local.el.")

(defconst user-frontpage-file
  (locate-user-emacs-file "frontpage.org")
  "Points to the file containing the startup message.")

(defun find-user-init-file ()
  "Edit `user-init-file' without opening a new window."
  (interactive)
  (find-file user-init-file))

(defun find-user-config-file ()
  "Edit `user-config-file' without opening a new window."
  (interactive)
  (find-file user-config-file))

(defun find-user-local-file ()
  "Edit `local.el' without opening a new window."
  (interactive)
  (find-file user-local-file))

(defun find-user-frontpage-file ()
  "Edit `user-frontpage-file' without opening a new window."
  (interactive)
  (find-file user-frontpage-file))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f seconds." (float-time (time-since time)))))

(defun bootstrap-package ()
  "Add package repositories and call `package-initialize'."
  (require 'package)
  (dolist (x '(("melpa"        . "https://melpa.org/packages/")
               ("melpa-2"      . "https://melpa.milkbox.net/packages/")
               ("melpa-stable" . "https://stable.melpa.org/packages/")
               ("elpy"         . "https://jorgenschaefer.github.io/packages/")
               ("org"          . "https://orgmode.org/elpa/")
               ("gnu"          . "https://elpa.gnu.org/packages/")
               ("marmalade"    . "https://marmalade-repo.org/packages/")))
    (add-to-list 'package-archives x))
  (when (< emacs-major-version 27)
    ;; package-initialize doesn't have to be called here in emacs 27
    (package-initialize)))

(defun bootstrap-straight ()
  "Load straight.el, downloading it if necessary.
`package-initialize' must be called prior to this."
  ;; Requires (package-initialize) to be called
  ;; https://github.com/raxod502/straight.el
  (let ((bootstrap-file
         (concat user-emacs-directory
                 "straight/repos/straight.el/bootstrap.el"))
        (bootstrap-version 3))
    (unless (file-exists-p bootstrap-file)
      (message "Bootstrapping Straight.el...")
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (let ((lockfile (locate-user-emacs-file "packages.el"))
        (profile-name 'personal))
    (when (file-exists-p lockfile)
      (customize-set-variable 'straight-profiles
                              (add-to-list 'straight-profiles
                                           (cons profile-name
                                                 lockfile)))
      (customize-set-variable 'straight-current-profile
                              profile-name)))
  (customize-set-variable 'straight-cache-autoloads t)
  (customize-set-variable 'straight-use-package-by-default t))

;; (defun bootstrap-quelpa ()
;;   ;; Requires (package-initialize) to be called beforehand
;;   (if (require 'quelpa nil t)
;;     (quelpa-self-upgrade))
;;   (with-temp-buffer
;;     (url-insert-file-contents
;;      "https://framagit.org/steckerhalter/quelpa/raw/master/bootstrap.el")
;;     (eval-buffer)))

;;; Use package
(defun bootstrap-use-package ()
  "Check if use-package is installed and install it if it isn't.
Then performs configuration of `use-package' variables."
  ;; (unless (featurep 'quelpa)
  ;;   (bootstrap-quelpa))
  ;; (quelpa
  ;;   '(quelpa-use-package
  ;;     :fetcher git
  ;;     :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))
  ;; (require 'quelpa-use-package)
  (unless (featurep 'straight)
    (bootstrap-straight))
  (require 'straight)
  (customize-set-variable 'load-prefer-newer t)
  (straight-use-package '(use-package
                           :host github
                           :repo "jwiegley/use-package"))
  (require 'use-package)
  ;; download packages if needed
  (customize-set-variable 'use-package-always-defer nil
                          "we don't always lazy load because of explicitness")
  (customize-set-variable 'use-package-always-ensure nil
                          "always make sure it never skips if not
                          found. Disabled because we want straight to
                          do the heavy lifting")
  (customize-set-variable 'use-package-verbose t)
  (customize-set-variable 'use-package-compute-statistics t)
  (customize-set-variable 'use-package-hook-name-suffix nil)
  (use-package use-package-ensure-system-package))

(defun load-local-el ()
  "Check if there exists a local.el file. Create one if it doesn't.
exist, using the template specified in
'auto-insert/elisp-local-template'. Then loads the file"
  (let ((local-file (locate-user-emacs-file "local.el")))
    (unless (file-exists-p local-file)
      ;; output a templated local.el file into local.el
      (message "local.el is currently unconfigured! Creating new local.el...")
      (write-region (with-temp-buffer
                      (insert-file-contents (locate-user-emacs-file
                                             "local-template.el"))
                      (buffer-string)) nil local-file))
    (load local-file)
    (when (fboundp 'my-after-init-code)
      (add-hook 'after-init-hook #'my-after-init-code))))

(defun load-config-org-files (files)
  "Given a list of org FILES, load them sequentially in the order.
specified The list of files is assumed to be relative to
`user-init-dir' TODO: Error checking; relative pathing, error
recovery. Maybe eventually load dependencies and all that."
  (dolist (file files)
    (message "Loading %s" file)
    (condition-case nil
        (org-babel-load-file (locate-user-emacs-file file))
      (error (message "There was an error when loading %s" file)))))

(defun my-straight-update-packages ()
  "When called, update all straight packages."
  (interactive)
  (require 'straight)
  (straight-pull-all)
  (straight-check-all)
  (straight-prune-build))

;; (defun my-bootstrap-el-get ()
;;   (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;;   (unless (require 'el-get nil 'noerror)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;;   (el-get 'sync))

(let ((gc-cons-threshold most-positive-fixnum))
  (bootstrap-package)
  (bootstrap-straight)
  ;; (bootstrap-quelpa)
  ;; (my-bootstrap-el-get)
  (bootstrap-use-package)

  ;; Load core configuration that I can't work without. Everything
  ;; else gets shoved into config.org except these.

  ;; https://github.com/emacscollective/auto-compile
  (use-package auto-compile
    :straight (:host github :repo "emacscollective/auto-compile")
    :custom
    (load-prefer-newer t)
    (auto-compile-verbose t)
    :config
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode))

  (use-package async
    :straight (:host github :repo "jwiegley/emacs-async")
    :config
    (async-bytecomp-package-mode 1))

  (use-package general
    :straight (:host github :repo "noctuid/general.el")
    :init
    (defconst my-default-evil-leader-key "SPC"))

  (use-package no-littering
    :straight (:host github :repo "emacscollective/no-littering"))

  (customize-set-value 'evil-want-keybinding nil
                       "`evil-collections' wants this to be
     disabled before even loading evil, see
     https://github.com/emacs-evil/evil-collection/issues/60")

  (use-package dash
    :straight t)

  (use-package f
    :straight t)

  (use-package s
    :straight t)

  (use-package ht
    :straight t)

  (use-package ts
    :straight t)

;;; Evil-mode
  (use-package evil
    :demand t
    :straight (:host github :repo "emacs-evil/evil")
    ;; :straight t
    :commands (evil-set-initial-state
               evil-insert-state
               evil-ex-define-cmd)
    :general
    ;; (global-map "C-u" nil) ;; Disable universal argument
    ;; (:keymaps 'insert
    ;;  "C-u"    'kill-whole-line
    ;;  "C-l"    'evil-complete-next-line)
    ;; (:keymaps 'motion
    ;; "C-u"    'evil-scroll-up)
    (:keymaps 'insert
     "C-l" 'evil-complete-next-line
     "C-u" 'evil-delete-whole-line)
    (:keymaps 'normal
     ;; try eyebrowse instead
     ;; "gt"     '(lambda () (interactive) (other-frame 1))
     ;; "gT"     '(lambda () (interactive) (other-frame -1))
     "g a" 'describe-char
     "g o" 'ff-find-other-file)
    ;; "g a"    'describe-char)
    (:keymaps 'inner
     "e"      'my-evil-a-buffer)
    (:keymaps 'outer
     "e"      'my-evil-a-buffer)
    :custom
    (evil-want-Y-yank-to-eol
     t
     "Y has the default behavior of functioning identically to yy.
     Change it to function similarly to dd, and cc instead. Equivalent
     to nnoremap yy y$")
    (evil-regexp-search
     t
     "Use regular expressions while searching instead of plaintext
     matching.")
    (evil-want-C-u-scroll
     t
     "In vim, <C-u> maps to half page up. In Emacs, it corresponds to
     the `universal-argument' function that might augment a function
     call. We prefer the scrolling.")
    (evil-split-window-below
     ;; t
     nil
     "`set splitbelow` in vim")
    (evil-vsplit-window-right
     ;; t
     nil
     "`set splitright` in vim")
    (evil-move-beyond-eol
     t
     "As recommended by evil-cleverparens")
    (evil-auto-indent
     t
     "Automatically indent when inserting a newline")
    (evil-want-fine-undo t)
    :hook (evil-normal-state-entry-hook . evil-ex-nohighlight)
    :config
    (defun update-evil-shift-width ()
      "We do this otherwise packages like parinfer would mess up with
        the indentation, since their default is 4 but lisp-mode defaults
        are generally 2."
      (require 'evil)
      (customize-set-variable 'evil-shift-width lisp-body-indent))

    ;; Back to our regularly scheduled programming
    (evil-select-search-module 'evil-search-module 'evil-search)

    ;; ;; https://emacs.stackexchange.com/questions/28135/in-evil-mode-how-can-i-prevent-adding-to-the-kill-ring-when-i-yank-text-visual
    ;; (let ((func (lambda (oldpaste &rest r)
    ;;               (interactive)
    ;;               (let ((evil-this-register ?0))
    ;;                 (call-interactively oldpaste)))))
    ;;   (advice-add 'evil-paste-before :around func)
    ;;   (advice-add 'evil-paste-after  :around func))

    ;; (evil-define-command ex-tab-edit(file)
    ;;   (interactive "P<f>")
    ;;   (raise-frame (call-interactively 'make-frame))
    ;;   (evil-edit file))

    (defmacro my-evil-define-split-vsplit-cmd (command body)
      "Does split and vsplit, but not tab (for now) since that
requires a different logic."
      (require 'evil)
      (let ((split-command-name (concat "S" command))
            (vsplit-command-name (concat "V" command)))
        `(progn
           (evil-ex-define-cmd ,command
                               #'(lambda () (interactive)
                                   (funcall-interactively ,body)))
           (evil-ex-define-cmd ,split-command-name
                               #'(lambda () (interactive)
                                   (call-interactively 'evil-window-split)
                                   (funcall-interactively ,body)))
           (evil-ex-define-cmd ,vsplit-command-name
                               #'(lambda () (interactive)
                                   (call-interactively 'evil-window-vsplit)
                                   (funcall-interactively ,body))))))

    (evil-ex-define-cmd "bc[lose]" 'kill-this-buffer)
    (evil-define-command my-evil-vsplit-buffer (&optional buffer)
      "Strictly speaking this isn't implemented in vim, which is why
we're adding a custom function for it here."
      :repeat nil
      (interactive "<b>")
      (evil-window-vsplit)
      (evil-buffer buffer))

    (evil-ex-define-cmd "vb[uffer]" 'my-evil-vsplit-buffer)

    (defun my-new-cmd-tab (dest)
      (interactive)
      (if (>= emacs-major-version 27)
          (let ((tab-bar-new-tab-choice dest))
            (tab-bar-new-tab))
        (require 'eyebrowse)
        (funcall-interactively 'my-new-evil-tab dest)))

    (evil-ex-define-cmd "frontpage" 'find-user-frontpage-file)
    (my-evil-define-split-vsplit-cmd "init" 'find-user-init-file)
    (evil-ex-define-cmd "Tinit" #'(lambda ()
                                    (interactive)
                                    (my-new-cmd-tab user-init-file)))
    (my-evil-define-split-vsplit-cmd "local" 'find-user-local-file)
    (evil-ex-define-cmd "Tlocal" #'(lambda ()
                                     (interactive)
                                     (my-new-cmd-tab user-local-file)))
    (my-evil-define-split-vsplit-cmd "config" 'find-user-config-file)
    (evil-ex-define-cmd "Tconfig" #'(lambda () (interactive)
                                      (my-new-cmd-tab user-config-file)))
    (my-evil-define-split-vsplit-cmd "buffers" 'ibuffer)
    (my-evil-define-split-vsplit-cmd "me[ssage]"
                                     #'(lambda ()
                                         (switch-to-buffer "*Messages*")))
    (my-evil-define-split-vsplit-cmd "sc[ratch]"
                                     #'(lambda ()
                                         (switch-to-buffer "*scratch*")))

    ;; (evil-ex-define-cmd "framen" 'make-frame)
    ;; (evil-ex-define-cmd "framec" 'delete-frame)

    ;; https://stackoverflow.com/questions/18102004/emacs-evil-mode-how-to-create-a-new-text-object-to-select-words-with-any-non-sp/22418983#22418983
    (defmacro /evil-define-and-bind-text-object (key start-regex end-regex)
      (let ((inner-name (make-symbol "inner-name"))
            (outer-name (make-symbol "outer-name")))
        `(progn
           (evil-define-text-object ,inner-name (count &optional beg end type)
             (evil-select-paren ,start-regex ,end-regex beg end type count nil))
           (evil-define-text-object ,outer-name (count &optional beg end type)
             (evil-select-paren ,start-regex ,end-regex beg end type count t))
           (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
           (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

    ;; ;; https://www.emacswiki.org/emacs/RegularExpression
    ;; (/evil-define-and-bind-text-object "/" "/" "/")
    ;; (/evil-define-and-bind-text-object "\\" "\\" "\\")
    ;; (/evil-define-and-bind-text-object "|" "|" "|")

    (evil-define-text-object my-evil-a-buffer (count &optional beg end type)
      "Select entire buffer"
      (evil-range (point-min) (point-max)))
    (evil-mode))

;;; evil-collection

  (use-package evil-collection
    :straight t
    ;;    :straight (:host github :repo "emacs-evil/evil-collection"
    ;;               :files (:defaults ("modes" "modes/*")))
    :custom
    (evil-collection-setup-minibuffer t)
    :config
    (evil-collection-init))

  ;; ;; https://github.com/raxod502/straight.el/blob/develop/README.md#installing-org-with-straightel
  ;; (progn
  ;;   (require 'subr-x)
  ;;   (straight-use-package 'git)

  ;;   (defun org-git-version ()
  ;;     "The Git version of org-mode.
  ;;     Inserted by installing org-mode or when a release is made."
  ;;     (require 'git)
  ;;     (let ((git-repo (expand-file-name
  ;;                      "straight/repos/org/" user-emacs-directory)))
  ;;       (string-trim
  ;;        (git-run "describe"
  ;;                 "--match=release\*"
  ;;                 "--abbrev=6"
  ;;                 "HEAD"))))

  ;;   (defun org-release ()
  ;;     "The release version of org-mode.
  ;;     Inserted by installing org-mode or when a release is made."
  ;;     (require 'git)
  ;;     (let ((git-repo (expand-file-name
  ;;                      "straight/repos/org/" user-emacs-directory)))
  ;;       (string-trim
  ;;        (string-remove-prefix
  ;;         "release_"
  ;;         (git-run "describe"
  ;;                  "--match=release\*"
  ;;                  "--abbrev=0"
  ;;                  "HEAD")))))

  ;;   (provide 'org-version))

  ;; ;; We do this here because we want a directory to actually exist when the
  ;; ;; next form gets evaluated
  ;; (straight-use-package 'org-plus-contrib)

;;; org-mode

  (use-package org-plus-contrib
    ;; (use-package org
    ;;   :ensure org-plus-contrib
    :commands (orgtbl-mode
               org-babel-load-file)
    :mode
    ("\\.todo\\'" . org-mode)
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
     "o T" '(lambda () (interactive)
              (org-time-stamp '(16))))
    ;; "f f" 'counsel-org-goto)
    (org-mode-map
     "C-c C-'" 'org-edit-special
     "<C-M-return>" 'org-insert-subheading
     "<C-M-S-return>" 'org-insert-todo-subheading)
    (org-src-mode-map
     "C-c C-'" 'org-src-edit-exit)
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
    (org-todo-keywords '((sequence "TODO(t)" "ONGOING(o)" "BLOCKED(b@)"
                                   "|" "DONE(d@)" "SKIPPED(s@)")))
    (org-enforce-todo-checkbox-dependencies
     nil "Sometimes we are able to skip dependencies as things happen")
    (org-enforce-todo-dependencies
     nil "Same reason as `org-enforce-todo-checkbox-dependencies'")
    (org-pretty-entities
     nil "It gets a bit annoying when you autocomplete braces")
    (org-log-done 'time)
    (org-log-note-clock-out t)
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
    (org-link-descriptive t)
    (org-adapt-indentation nil "Maintaining indentation for org-files
    looks annoying when editing it as a plain text file")
    ;; (org-list-indent-offset 1)
    ;; (org-extend-today-until
    ;;  5 "I think 5 am is a safe bet for the end of the day")
    (org-note-done 'note)
    :custom-face
    (org-checkbox ((t (:bold t :box nil))))
    :hook ((org-insert-heading-hook . evil-insert-state))
    :init
    (unless (display-graphic-p)
      (general-define-key
       :keymaps 'org-mode-map
       :states '(normal insert motion)
       ;; "C-^" 'org-insert-heading-after-current
       "C-^" 'org-meta-return
       "\236" 'org-insert-todo-heading-respect-content))
    ;; (with-eval-after-load 'org
    ;;   (add-hook 'org-mode-hook '(lambda ()
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
    :config
    (defun my-org-convert-list-to-checkbox ()
      (when (and (org-at-item-p)
                 (not (org-at-item-checkbox-p)))
        (org-toggle-checkbox '(4))))
    ;; NOTE: for some reason, this hook is not being run
    (add-hook 'org-ctrl-c-ctrl-c-final-hook 'my-org-convert-list-to-checkbox)
    ;; NOTE: this is a hack, because I've learnt that this hook is not
    ;; consistently being called.
    (advice-add 'org-ctrl-c-ctrl-c :after
                #'(lambda (&rest _) (run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-final-hook)))
    (with-eval-after-load 'evil
      ;; NOTE: define our own hacked evil-fill and evil-fill-and-move so it will work on list items
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
                '(lambda ()
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

  ;; https://github.com/zzamboni/dot-emacs/blob/master/init.org#cheatsheet-and-experiments


  (use-package ivy
    :demand t
    :straight (:host github :repo "abo-abo/swiper")
    :general
    (ivy-minibuffer-map
     "M-j" 'ivy-next-line
     "M-k" 'ivy-previous-line
     "<C-return>" 'ivy-immediate-done)
    (:states 'normal
     :prefix my-default-evil-leader-key
     "<SPC>" 'counsel-M-x)
    ("C-h C-h" 'counsel-apropos)
    :custom
    (ivy-use-selectable-prompt
     t
     "Make the prompt line selectable.")
    (ivy-use-virtual-buffers
     t
     "Make `ivy-switch-buffer' look more like `helm-mini'")
    :init
    (evil-ex-define-cmd "bb" 'counsel-buffer-or-recentf)
    :config
    (ivy-mode))

  (use-package restart-emacs
    :straight (:host github :repo "iqbalansari/restart-emacs")
    :commands (restart-emacs)
    :init
    (evil-ex-define-cmd "restart" 'restart-emacs))

  ;; Load local configuration variables, we do it here so that
  ;; local.el gets access to the "core" init loads
  (load-local-el)

  ;;NOTE: Do *NOT* compile this, certain macro definitions won't get compiled
  ;;and the init load will fail
  (org-babel-load-file (locate-user-emacs-file "config.org"))

  (add-hook 'after-init-hook
            #'(lambda ()
                (message "Loaded .emacs.d in %.06f seconds."
                         (float-time (time-since my-init-start-time)))))

  ;; Disable ANNOYING customize options
  (setq custom-file (locate-user-emacs-file "custom.el")))
  ;; (setq custom-file (make-temp-file "")))
(message "Configuration complete.")

(org-agenda-list)

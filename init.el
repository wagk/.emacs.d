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

(setq user-full-name    "Pang Tun Jiang"
      user-mail-address "pang.tun.jiang@gmail.com")

;; buffer encoding systems
;; We do this here because the package system might need to know our preferences
(set-charset-priority 'unicode)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(customize-set-value 'locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

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
  "Add package repositories and calls `package-initialize'."
  (require 'package)
  (dolist (x '(("melpa"        . "https://melpa.org/packages/")
               ("melpa-2"      . "https://melpa.milkbox.net/packages/")
               ("melpa-stable" . "https://stable.melpa.org/packages/")
               ("elpy"         . "https://jorgenschaefer.github.io/packages/")
               ("org"          . "https://orgmode.org/elpa/")
               ("gnu"          . "https://elpa.gnu.org/packages/")
               ("marmalade"    . "https://marmalade-repo.org/packages/")))
    (add-to-list 'package-archives x))
  (package-initialize))

(defun bootstrap-straight ()
  "Load straight.el, downloading it if necessary.
`package-initialize' must be called prior to this."
  ;; Requires (package-initialize) to be called
  ;; https://github.com/raxod502/straight.el
  (let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
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
  (setq straight-cache-autoloads t))

;; (defun bootstrap-quelpa ()
;;   ;; Requires (package-initialize) to be called beforehand
;;   (if (require 'quelpa nil t)
;;     (quelpa-self-upgrade))
;;   (with-temp-buffer
;;     (url-insert-file-contents
;;      "https://framagit.org/steckerhalter/quelpa/raw/master/bootstrap.el")
;;     (eval-buffer)))

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
  (setq use-package-always-defer nil ;; we don't always lazy load because of explicitness
        use-package-always-ensure nil ;; always make sure it never skips if not found. Disabled because we want straight to do the heavy lifting
        use-package-verbose t
        use-package-compute-statistics t)
  (use-package use-package-ensure-system-package))

(defun load-local-el ()
  "Check if there exists a local.el file. Create one if it doesn't.
exist, using the template specified in
'auto-insert/elisp-local-template'. Then loads the file"
  (let ((local-file (locate-user-emacs-file "local.el")))
    (unless (file-exists-p local-file)
      ;; output a templated local.el file into local.el
      (write-region (with-temp-buffer
                      (insert-file-contents (locate-user-emacs-file "local-template.el"))
                      (buffer-string)) nil local-file))
    (load local-file)))

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

(let ((gc-cons-threshold most-positive-fixnum))
  (bootstrap-package)
  (bootstrap-straight)
  ;; (bootstrap-quelpa)
  (bootstrap-use-package)

  ;; Load local configuration variables
  (load-local-el)

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

  (use-package hydra
    :straight (:host github :repo "abo-abo/hydra")
    :config
    (use-package use-package-hydra
      :straight (:host gitlab :repo "to1ne/use-package-hydra")))

  (use-package no-littering
    :straight (:host github :repo "emacscollective/no-littering"))

  (customize-set-value 'evil-want-keybinding nil
    "`evil-collections' wants this to be
     disabled before even loading evil, see
     https://github.com/emacs-evil/evil-collection/issues/60")

  (use-package evil
    :demand t
    :straight (:host github :repo "emacs-evil/evil")
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
     "g o"    'ff-find-other-file)
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
     t
     "`set splitbelow` in vim")
    (evil-vsplit-window-right
     t
     "`set splitright` in vim")
    (evil-move-beyond-eol
     t
     "As recommended by evil-cleverparens")
    (evil-auto-indent
     t
     "Automatically indent when inserting a newline")
    :hook (evil-normal-state-entry . evil-ex-nohighlight)
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

    (evil-ex-define-cmd "bc[lose]"    'kill-this-buffer)

    (evil-ex-define-cmd "sh[ell]" 'shell) ;; at least shell shows its keymaps
    (evil-ex-define-cmd "init" 'find-user-init-file)
    (evil-ex-define-cmd "Sinit" '(lambda () (interactive)
                                   (call-interactively 'evil-window-split)
                                   (find-user-init-file)))
    (evil-ex-define-cmd "Vinit" '(lambda () (interactive)
                                   (call-interactively 'evil-window-vsplit)
                                   (find-user-init-file)))
    (evil-ex-define-cmd "local" 'find-user-local-file)
    (evil-ex-define-cmd "Slocal" '(lambda () (interactive)
                                    (call-interactively 'evil-window-split)
                                    (find-user-local-file)))
    (evil-ex-define-cmd "Vlocal" '(lambda () (interactive)
                                    (call-interactively 'evil-window-vsplit)
                                    (find-user-local-file)))
    (evil-ex-define-cmd "frontpage" 'find-user-frontpage-file)
    (evil-ex-define-cmd "config" 'find-user-config-file)
    (evil-ex-define-cmd "Sconfig" '(lambda () (interactive)
                                     (call-interactively 'evil-window-split)
                                     (find-user-config-file)))
    (evil-ex-define-cmd "Vconfig" '(lambda () (interactive)
                                     (call-interactively 'evil-window-vsplit)
                                     (find-user-config-file)))
    (evil-ex-define-cmd "me[ssage]" '(lambda () (interactive)
                                       (switch-to-buffer "*Messages*")))
    (evil-ex-define-cmd "Smessage" '(lambda () (interactive)
                                     (call-interactively 'evil-window-split)
                                     (switch-to-buffer "*Messages*")))
    (evil-ex-define-cmd "Vmessage" '(lambda () (interactive)
                                     (call-interactively 'evil-window-vsplit)
                                     (switch-to-buffer "*Messages*")))
    (evil-ex-define-cmd "sc[ratch]"   '(lambda () (interactive) (switch-to-buffer "*scratch*")))
    (evil-ex-define-cmd "Sscratch" '(lambda () (interactive)
                                     (call-interactively 'evil-window-split)
                                     (switch-to-buffer "*scratch*")))
    (evil-ex-define-cmd "Vscratch" '(lambda () (interactive)
                                     (call-interactively 'evil-window-vsplit)
                                     (switch-to-buffer "*scratch*")))

    (evil-ex-define-cmd "framen" 'make-frame)
    (evil-ex-define-cmd "framec" 'delete-frame)

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

  (use-package evil-collection
    :straight (:host github :repo "emacs-evil/evil-collection")
    :custom
    (evil-collection-setup-minibuffer t)
    :config
    (evil-collection-init))

  ;; (straight-use-package '(org :local-repo nil))

  ;; https://github.com/raxod502/straight.el/blob/develop/README.md#installing-org-with-straightel
  (progn
    (require 'subr-x)
    (straight-use-package 'git)

    (defun org-git-version ()
      "The Git version of org-mode.
      Inserted by installing org-mode or when a release is made."
      (require 'git)
      (let ((git-repo (expand-file-name
                        "straight/repos/org/" user-emacs-directory)))
        (string-trim
          (git-run "describe"
                  "--match=release\*"
                  "--abbrev=6"
                  "HEAD"))))

    (defun org-release ()
      "The release version of org-mode.
      Inserted by installing org-mode or when a release is made."
      (require 'git)
      (let ((git-repo (expand-file-name
                        "straight/repos/org/" user-emacs-directory)))
        (string-trim
          (string-remove-prefix
            "release_"
            (git-run "describe"
                    "--match=release\*"
                    "--abbrev=0"
                    "HEAD")))))

    (provide 'org-version))

  ;; We do this here because we want a directory to actually exist when the
  ;; next form gets evaluated
  (straight-use-package 'org-plus-contrib)

  (use-package org-plus-contrib
    ;; doesn't have a straight recipe because it relies on make or something
    ;; :ensure org-plus-contrib
    ;; :straight (:repo "https://code.orgmode.org/bzg/org-mode.git"
    ;;            :local-repo "org"
    ;;            :files (:defaults "contrib/lisp/*.el")
    ;;            :includes (org))
    ;; https://github.com/raxod502/straight.el#installing-org-with-straightel
    ;; :straight (:repo "https://code.orgmode.org/bzg/org-mode.git"
    ;;            :local-repo "org"
    ;;            :files (:defaults "contrib/lisp/*.el")
    ;;            :includes org)
    ;; :straight (:local-repo "org"
    ;;            :files (:defaults "contrib/lisp/*.el"))
    ;; :straight (:includes org)
    :commands (orgtbl-mode
               org-babel-load-file)
    :mode
    ("\\.todo\\'" . org-mode)
    :general
    (:states  '(normal motion)
     :keymaps 'org-mode-map
     "TAB"    'org-cycle
     "<tab>"  'org-cycle
     "g x"    'org-open-at-point)
    ;; (:states 'insert
    ;;  :keymaps 'org-mode-map
    ;;  "TAB"    'hippie-expand
    ;;  "<tab>"  'hippie-expand)
    (:states 'normal
     :prefix my-default-evil-leader-key
     "o t" 'org-time-stamp
     "o T" '(lambda () (interactive)
              (org-time-stamp '(16)))
     "f f" 'counsel-org-goto)
    ;; (:states 'normal
    ;;  :keymaps 'org-mode-map
    ;;  :prefix my-default-evil-leader-key
    ;;  "l l" 'org-toggle-latex-fragment
    ;;  "r r" 'org-refile
    ;;  "a a" 'org-archive-subtree)
    (org-mode-map
     "C-S-c C-S-c" '(lambda () (interactive)
                      (org-toggle-checkbox '(4)))
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
    (org-startup-indented t)
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
    (org-enforce-todo-checkbox-dependencies
     nil "Sometimes we are able to skip dependencies as things happen")
    (org-enforce-todo-dependencies
     nil "Same reason as `org-enforce-todo-checkbox-dependencies'")
    (org-pretty-entities
     nil "It gets a bit annoying when you autocomplete braces")
    (org-log-done       'time)
    (org-log-redeadline 'time)
    (org-log-reschedule 'time)
    (org-blank-before-new-entry '((heading         . t)
                                  (plain-list-item . nil)))
    (org-refile-targets '((nil . (:maxlevel . 9))))
    (org-refile-use-outline-path t)
    (org-outline-path-complete-in-steps nil)
    (org-refile-allow-creating-parent-nodes 'confirm)
    (org-highlight-latex-and-related '(latex))
    (org-insert-heading-respect-content nil)
    (org-catch-invisible-edits 'show-and-error)
    (org-cycle-separator-lines 0)
    ;; (org-list-indent-offset 1)
    (org-extend-today-until
     5 "I think 5 am is a safe bet for the end of the day")
    (org-note-done 'note)
    :hook ((org-insert-heading . evil-insert-state))
    :config
    ;; (with-eval-after-load 'smartparens
    ;; make smartparen autoskip "" because org-mode treats it as a string
    ;; (sp-local-pair 'org-mode "\"" nil :when '(:rem sp-in-string-p))
    ;; (sp-local-pair 'org-mode "$" "$"))
    ;; https://github.com/zzamboni/dot-emacs/blob/master/init.org#cheatsheet-and-experiments
    (defun my-org-reformat-buffer ()
      (interactive)
      (when (y-or-n-p "Really format current buffer? ")
        (let ((document (org-element-interpret-data (org-element-parse-buffer))))
          (erase-buffer)
          (insert document)
          (goto-char (point-min)))))
    (use-package ox-confluence
      :ensure nil
      :commands org-confluence-export-as-confluence)
    (add-to-list 'org-babel-load-languages '(shell . t)))

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
    (evil-ex-define-cmd "bb" 'ivy-switch-buffer)
    :config
    (ivy-mode))

  (use-package restart-emacs
    :straight (:host github :repo "iqbalansari/restart-emacs")
    :commands (restart-emacs)
    :init
    (evil-ex-define-cmd "restart" 'restart-emacs))

  ;;NOTE: Do *NOT* compile this, certain macro definitions won't get compiled
  ;;and the init load will fail
  (measure-time
   (org-babel-load-file (locate-user-emacs-file "config.org")))

  ;; Disable ANNOYING customize options
  ;; (setq custom-file (locate-user-emacs-file "custom.el")))
  (setq custom-file (make-temp-file "")))

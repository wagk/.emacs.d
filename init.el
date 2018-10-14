;;; init.el --- Bootstrap further configurations

;;; Commentary:
;; Useful resources:
;; https://bling.github.io/blog/2013/10/27/emacs-as-my-leader-vim-survival-guide/
;; https://github.com/bbatsov/emacs-lisp-style-guide
;; https://github.com/noctuid/evil-guide
;; TODO: Figure out why there is a eager-macro expansion failure

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
(prefer-coding-system 'utf-8)

(defconst user-init-dir
  (file-name-as-directory
   (cond ((boundp 'user-emacs-directory)
          user-emacs-directory)
         ((boundp 'user-init-directory)
          user-init-directory)
         (t "~/.emacs.d/")))
  "Sets up the startup directory.")

(defun at-user-init-dir (filename)
  "Convenience function for files that path relative to `user-init-dir'"
  (expand-file-name (concat user-init-dir filename)))

(defconst user-init-file
  (at-user-init-dir "init.el")
  "Points to init.el")

(defconst user-config-file
  (at-user-init-dir "config.org")
  "Points to config.org")

(defconst user-local-file
  (at-user-init-dir "local.el")
  "Points to local.el")

(defconst user-config-file-list
  '("core.org"
    "lang.org")
  "List of config files that are to be loaded. Load order is the
  sequence defined within the list")

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

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f seconds." (float-time (time-since time)))))

(defun bootstrap-package ()
  "Adds package repositories and calls `package-initialize'"
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa-2" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
  (add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")) ; https://marmalade-repo.org/packages/#windowsinstructions
  (package-initialize))

(defun bootstrap-straight ()
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

(defun bootstrap-use-package ()
  "Checks if use-package is installed and installs it if it isn't.
  Then performs configuration of use-package variables"
  (unless (featurep 'straight)
    (my-bootstrap-straight))
  (require 'straight)
  (customize-set-variable 'load-prefer-newer t)
  (straight-use-package '(use-package
                           :type git
                           :host github
                           :repo "jwiegley/use-package"
                           :branch "master"))
  (require 'use-package)
  ;; download packages if needed
  (setq use-package-always-defer nil ;; we don't always lazy load because of explicitness
        use-package-always-ensure nil ;; always make sure it never skips if not found. Disabled because we want straight to do the heavy lifting
        use-package-verbose t
        use-package-compute-statistics t)
  (use-package diminish)
  (use-package bind-key))

(defun load-local-el ()
  "Checks if there exists a local.el file. Creates one if it doesn't
exist, using the template specified in
'auto-insert/elisp-local-template'. Then loads the file"
  (let ((local-file (at-user-init-dir "local.el")))
    (unless (file-exists-p local-file)
      ;; output a templated local.el file into local.el
      (write-region (with-temp-buffer
                      (insert-file-contents (at-user-init-dir)))
                    "auto-insert/elisp-local-template"
                    (buffer-string)
                    nil local-file))
    (load local-file)))

(defun load-config-org-files (files)
  "Given a list of org files, loads them sequentially in the order
specified The list of files is assumed to be relative to
`user-init-dir' TODO: Error checking; relative pathing, error
recovery. Maybe eventually load dependencies and all that."
  (dolist (file files)
    (message "Loading %s" file)
    (condition-case nil
        (org-babel-load-file (at-user-init-dir file))
      (error (message "There was an error when loading %s" file)))))

(let ((gc-cons-threshold most-positive-fixnum))
  (bootstrap-package)
  (bootstrap-straight)
  (bootstrap-use-package)

  ;; Load local configuration variables
  (load-local-el)

  ;; Load core configuration that I can't work without. Everything
  ;; else gets shoved into config.org except these.

  ;; https://github.com/emacscollective/auto-compile
  (use-package auto-compile
    :straight (:host github :repo "emacscollective/auto-compile" :branch "master")
    :custom
    (load-prefer-newer t)
    (auto-compile-verbose t)
    :config
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode))

  (use-package async
    :straight (:host github :repo "jwiegley/emacs-async" :branch "master")
    :config
    (async-bytecomp-package-mode 1))

  (use-package general
    :straight (:host github :repo "noctuid/general.el" :branch "master")
    :init
    (defconst my-default-evil-leader-key "SPC"))

  (use-package no-littering
    :straight (:host github :repo "emacscollective/no-littering" :branch "master"))

  (use-package evil
    :demand t
    :straight (:host github :repo "emacs-evil/evil" :branch "master")
    :commands (evil-set-initial-state
               evil-insert-state
               evil-ex-define-cmd)
    :general
    (global-map "C-u" nil) ;; Disable universal argument
    (:keymaps 'insert
     "C-u"    'kill-whole-line
     "C-l"    'evil-complete-next-line)
    (:keymaps 'motion
     "C-u"    'evil-scroll-up)
    (:keymaps 'normal
     "gt"     '(lambda () (interactive) (other-frame 1))
     "gT"     '(lambda () (interactive) (other-frame -1))
     "g o"    'ff-find-other-file
     "g a"    'describe-char)
    (:keymaps 'inner
     "e"      'my-evil-a-buffer)
    (:keymaps 'outer
     "e"      'my-evil-a-buffer)
    :custom
    (evil-want-C-u-scroll
     t
     "Emacs uses `C-u' for its `universal-argument' function. It
     conflicts with scroll up in evil-mode")
    (evil-want-keybinding
     nil
     "`evil-collections' wants this to be
     disabled, see https://github.com/emacs-evil/evil-collection/issues/60")
    (evil-want-Y-yank-to-eol
     t
     "Y has the default behavior of functioning identically to yy.
     Change it to function similarly to dd, and cc instead.")
    (evil-regexp-search
     t
     "Use regular expressions while searching instead of plaintext
     matching.")
    (evil-want-C-u-scroll
     t
     "In vim, <C-u> maps to half page up. In Emacs, it corresponds to
     a universal argument that might augment a function call. We
     prefer the scrolling.")
    (evil-split-window-below
     t
     "`set splitbelow` in vim")
    (evil-vsplit-window-right
     t
     "`set splitright` in vim")
    (evil-auto-indent
     t
     "Automatically indent when inserting a newline")
    :config
    (defun update-evil-shift-width ()
      "We do this otherwise packages like parinfer would mess up with
        the indentation, since their default is 4 but lisp-mode defaults
        are generally 2."
      (interactive)
      (require 'evil)
      (customize-set-variable 'evil-shift-width lisp-body-indent))

    ;; Back to our regularly scheduled programming
    ;;(fset 'evil-visual-update-x-selection 'ignore)
    (evil-select-search-module 'evil-search-module 'evil-search)

    (evil-ex-define-cmd "sh[ell]"    'shell) ;; at least shell shows its keymaps
    (evil-ex-define-cmd "tabn[ew]"   'make-frame)
    (evil-ex-define-cmd "tabe[dit]"  'make-frame)
    (evil-ex-define-cmd "qw[indow]"  'delete-frame)
    (evil-ex-define-cmd "restart"    'restart-emacs)
    (evil-ex-define-cmd "init"       'find-user-init-file)
    (evil-ex-define-cmd "local"      'find-user-local-file)
    (evil-ex-define-cmd "me[ssage]"  '(lambda () (interactive) (switch-to-buffer "*Messages*")))
    (evil-ex-define-cmd "sc[ratch]"  '(lambda () (interactive) (switch-to-buffer "*scratch*")))
    (evil-ex-define-cmd "config"     'find-user-config-file)

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

    ;; https://www.emacswiki.org/emacs/RegularExpression
    (/evil-define-and-bind-text-object "/" "/" "/")
    (/evil-define-and-bind-text-object "\\" "\\" "\\")
    (/evil-define-and-bind-text-object "|" "|" "|")

    (evil-define-text-object my-evil-a-buffer (count &optional beg end type)
      "Select entire buffer"
      (evil-range (point-min) (point-max)))

    (add-hook 'evil-normal-state-entry-hook 'evil-ex-nohighlight)
    (evil-mode))

  (use-package helm
    :straight (:host github :repo "emacs-helm/helm" :branch "master")
    :general
    ("C-h C-h" 'helm-apropos
     "C-h h"   'helm-apropos)
    (:states 'normal
     "-" 'helm-find-files) ;; emulate vim-vinegar
    (:states 'normal
     :prefix my-default-evil-leader-key
     "<SPC>" 'helm-M-x
     "TAB"   'helm-resume
     "y y"   'helm-show-kill-ring
     "b b"   'helm-mini
     "m m"   'helm-bookmarks)
    (:keymaps 'helm-map
     "C-w" 'evil-delete-backward-word
     "\\"  'helm-select-action
     "C-j" 'helm-next-line
     "C-k" 'helm-previous-line
     "C-d" 'helm-next-page
     "C-u" 'helm-previous-page
     "C-l" 'helm-next-source
     "C-h" 'helm-previous-source
     "TAB" 'helm-execute-persistent-action)
    :init
    (evil-ex-define-cmd "bb" 'helm-mini)
    (evil-ex-define-cmd "book[marks]" 'helm-bookmarks)
    (evil-ex-define-cmd "bm" 'helm-bookmarks)
    :custom
    (helm-idle-delay 0.0)
    (helm-input-idle-delay 0.01)
    (helm-quick-update t)
    (helm-recentf-fuzzy-match t)
    (helm-locate-fuzzy-match nil) ;; locate fuzzy is worthless
    (helm-m-x-fuzzy-match t)
    (helm-buffers-fuzzy-matching t)
    (helm-semantic-fuzzy-match t)
    (helm-apropos-fuzzy-match t)
    (helm-imenu-fuzzy-match t)
    (helm-lisp-fuzzy-completion t)
    (helm-completion-in-region-fuzzy-match t)
    (helm-split-window-in-side-p t)
    (helm-use-frame-when-more-than-two-windows nil)
    :config
    (progn (helm-autoresize-mode)
           (setq helm-autoresize-min-height 40 ;; these values are %
                 helm-autoresize-max-height 40))
    (helm-mode))

  (use-package org
    :defer 1
    ;; doesn't have a straight recipe because it relies on make or something
    :commands (org-mode
               orgtbl-mode
               org-time-stamp-inactive
               org-refile)
    :general
    (:states 'normal
     :keymaps 'org-mode-map
     "TAB"    'org-global-cycle
     "<tab>"  'org-global-cycle)
    (:states 'normal
     :prefix my-default-evil-leader-key
     "o t" 'org-time-stamp-inactive
     "o T" '(lambda () (interactive)
              (org-time-stamp-inactive '(16))))
    (:states 'normal
     :keymaps 'org-mode-map
     :prefix my-default-evil-leader-key
     "r r" 'org-refile
     "R R" 'org-archive-subtree)
    (org-mode-map
     "C-c C-'" 'org-edit-special)
    (org-src-mode-map
     "C-c C-'" 'org-src-edit-exit)
    :custom
    (org-support-shift-select t
                              "Let me use J in org-mode please.")
    (org-startup-indented t)
    (org-indent-mode-turns-on-hiding-stars t)
    (org-src-tab-acts-natively t)
    (org-src-window-setup
     'current-window
     "I tend to have documentation/other things on adjacent windows")
    (org-src-fontify-natively t)
    (org-default-notes-file "~/TODO.org")
    (org-M-RET-may-split-line nil)
    (org-enforce-todo-checkbox-dependencies
     nil
     "Sometimes we are able to skip dependencies as things happen")
    (org-enforce-todo-dependencies          nil)
    (org-pretty-entities                    nil)
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
    ;; (org-src-block-faces `(("emacs-lisp" (:foreground ,my-solarized-dark-base0))))
    :config
    ;; when inserting a heading immediately go into insert mode
    (add-hook 'org-insert-heading-hook 'evil-insert-state)
    ;; make smartparen autoskip "" because org-mode treats it as a string
    (add-hook 'smartparens-mode-hook '(lambda ()
                                        (sp-local-pair 'org-mode "\"" nil
                                                       :when '(:rem sp-in-string-p))))

    ;; (defun my-add-org-evil-embrace-pairs ()
    ;;   "Add additional pairings that evil-surround doesn't cover"
    ;;   (require 'evil-embrace)
    ;;   (let ((org-pairs '((?= "=" . "=") ;; verbatim
    ;;                      (?* "*" . "*") ;; bold
    ;;                      (?_ "_" . "_") ;; underline
    ;;                      (?+ "+" . "+") ;; strikethrough
    ;;                      (?~ "~" . "~") ;; code
    ;;                      (?/ "/" . "/")))) ;; italic
    ;;     (dolist (pair org-pairs)
    ;;       (embrace-add-pair (car pair) (cadr pair) (cddr pair)))))
    ;; (add-hook 'org-mode-hook 'my-add-org-evil-embrace-pairs)

    ;; https://github.com/zzamboni/dot-emacs/blob/master/init.org#cheatsheet-and-experiments
    (defun my-org-reformat-buffer ()
      (interactive)
      (when (y-or-n-p "Really format current buffer? ")
        (let ((document (org-element-interpret-data (org-element-parse-buffer))))
          (erase-buffer)
          (insert document)
          (goto-char (point-min))))))

  ;;NOTE: Do *NOT* compile this, certain macro definitions won't get compiled
  ;;and the init load will fail
  (measure-time
   (org-babel-load-file (at-user-init-dir "config.org")))

  ;; (straight-freeze-versions)

  ;; Disable ANNOYING customize options
  (setq custom-file (at-user-init-dir "custom.el"))
  (load custom-file 'noerror))

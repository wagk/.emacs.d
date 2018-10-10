(use-package diminish)
(use-package bind-key)

;; https://github.com/noctuid/general.el
(use-package general
  :demand t
  :straight (:host github :repo "noctuid/general.el" :branch "master")
  :commands (general-define-key)
  :init
  (defconst my-default-evil-leader-key "SPC"))

;; (use-package use-package-ensure-system-package)

;; https://github.com/emacscollective/auto-compile
(use-package auto-compile
  :demand t
  :custom
  (load-prefer-newer t)
  (auto-compile-verbose t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package auto-package-update
  :commands (auto-package-update-now
             auto-package-update-at-time
             auto-package-update-maybe)
  :custom
  (auto-package-update-delete-old-versions t "We already version them on git")
  (auto-package-update-prompt-before-update t "NO SURPRISES")
  (auto-package-update-interval 14 "update once every 2 weeks (the count is in days)"))

;; local configuration variables
(progn (my-ensure-local-el-file-exists)
       (load (at-user-init-dir "local.el")))

(use-package evil
  :demand t
  :straight (:host github
	     :repo "emacs-evil/evil"
	     :branch "master")
  :commands (evil-set-initial-state
	     evil-insert-state
	     evil-ex-define-cmd)
  :general
  (global-map
   "C-u" nil) ;; Disable universal argument
  (:keymaps 'insert
   "C-u"    'kill-whole-line
   "C-l"    'evil-complete-next-line)
  (:keymaps 'motion
   "C-u"    'evil-scroll-up)
  (:keymaps 'normal
   "Y"      '/evil-copy-to-end-of-line
   "gt"     '/evil-gt
   "gT"     '/evil-gT
   "C-\\"   '/lang-toggle ;; binding for eng <-> jap
   "g o"    'ff-find-other-file
   "g a"    'describe-char)
  (:keymaps 'inner
   "/"      '/inner-forward-slash)
  (:keymaps 'outer
   "e"      'my-evil-a-buffer
   "/"      '/a-forward-slash)
  (:keymaps 'minibuffer-local-map
   "C-w"    'backward-kill-word)
  :custom
  (evil-want-C-u-scroll t
			"Emacs uses `C-u' for its `universal-argument' function.
				 It conflicts with scroll up in evil-mode")
  (evil-want-integration nil
			 "`evil-collections' demands that this be disabled to
				  work")
  (evil-want-keybinding nil
			"`evil-collections' wants this to be disabled,
			https://github.com/emacs-evil/evil-collection/issues/60")
  :config
;;;###autoload
  (defun /treat-underscore-as-word ()
    "Make underscore be considered part of a word, just like vim.
	 Add this to whichever mode you want when you want it to treat underscore as a
	 word"
    (modify-syntax-entry ?_ "w"))
;;;###autoload
  (defun /evil-gt ()
    "Emulating vim's `gt' using frames."
    (interactive)
    (other-frame 1))
;;;###autoload
  (defun /evil-gT ()
    "Emulating vim's `gT' using frames."
    (interactive)
    (other-frame -1))

  (defun my-lispy-evil-shift-width ()
    (customize-set-variable 'evil-shift-width lisp-body-indent))

  ;; Back to our regularly scheduled programming
  (fset 'evil-visual-update-x-selection 'ignore)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (setq evil-want-Y-yank-to-eol t
	sentence-end-double-space nil
	evil-regexp-search t
	evil-normal-state-modes (append evil-motion-state-modes
					evil-normal-state-modes)
	evil-motion-state-modes nil
	evil-want-C-u-scroll t
	evil-split-window-below t
	evil-vsplit-window-right t)
  (setq-default evil-auto-indent t)

  (add-hook 'prog-mode-hook #'/treat-underscore-as-word)



  (evil-ex-define-cmd "sh[ell]"    'shell) ;; at least shell shows its keymaps
  (evil-ex-define-cmd "tabn[ew]"   'make-frame)
  (evil-ex-define-cmd "tabe[dit]"  'make-frame)
  (evil-ex-define-cmd "qw[indow]"  'delete-frame)
  (evil-ex-define-cmd "restart"    'restart-emacs)
  (evil-ex-define-cmd "init"       'find-user-init-file)
  (evil-ex-define-cmd "local"      'find-user-local-file)
  (evil-ex-define-cmd "me[ssage]"  'find-message-buffer)
  (evil-ex-define-cmd "config"     'find-user-config-file)

  ;; nmap Y y$
  (defun /evil-copy-to-end-of-line ()
    "Yanks everything from point to the end of the line"
    (interactive)
    (evil-yank (point) (point-at-eol)))

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
  :defer 2
  :commands (helm-mini)
  :straight (:host github :repo "emacs-helm/helm" :branch "master")
  :general
  ("C-h C-h" 'helm-apropos
   "C-h h"   'helm-apropos)
  (:states 'normal
   "-"     'helm-find-files) ;; emulate vim-vinegar
  (:states  'normal
   :prefix my-default-evil-leader-key
   "<SPC>"  'helm-M-x
   "TAB"    'helm-resume
   "y y"    'helm-show-kill-ring
   "b b"    'helm-mini
   "m m"    'helm-bookmarks)
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
  :config
  (setq helm-idle-delay 0.0
        helm-input-idle-delay 0.01
        helm-quick-update t
	helm-recentf-fuzzy-match t
	helm-locate-fuzzy-match nil ;; locate fuzzy is worthless
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-semantic-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-lisp-fuzzy-completion t
        helm-completion-in-region-fuzzy-match t
        helm-split-window-in-side-p t
        helm-use-frame-when-more-than-two-windows nil)
  (progn (helm-autoresize-mode)
         (setq helm-autoresize-min-height 40 ;; these values are %
               helm-autoresize-max-height 40))
  (helm-mode))

(use-package restart-emacs
  :straight (:host github :repo "iqbalansari/restart-emacs" :branch "master")
  :commands (restart-emacs))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(window-divider-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-screen t)

(setq-default require-final-newline t)

(setq ring-bell-function 'ignore)

(setq w32-pipe-read-delay 0)

(setq tab-always-indent 'complete)

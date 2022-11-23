(require 'use-package)

(customize-set-value 'evil-want-keybinding nil
                     "`evil-collections' wants this to be disabled
                     before even loading evil, see
                     https://github.com/emacs-evil/evil-collection/issues/60")

;; (use-package undo-fu
;;   :straight t
;;   :when (< 28 emacs-major-version))

;; NOTE: reddit notes that there might still be some history
;; corruption?
(use-package undo-tree
  :straight t
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-auto-save-history nil) ;; the perf of this seems intensely bad
  (undo-tree-enable-undo-in-region t)
  :config
  (global-undo-tree-mode)
  (with-eval-after-load 'evil
    (general-define-key
      :states 'normal
      "U"     'undo-tree-visualize)))

;; Needed for g; and g,
(use-package goto-chg
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
  (:states 'normal
    :prefix my-default-evil-leader-key
    "<SPC>" 'execute-extended-command)
  (:keymaps 'visual
    "J" 'evil-join)
  (:keymaps 'insert
    "C-l" 'evil-complete-next-line
    "C-u" 'evil-delete-whole-line)
  (:keymaps 'normal
    "g C-u" 'universal-argument
    ;; try eyebrowse instead
    ;; "gt"     '(lambda () (interactive) (other-frame 1))
    ;; "gT"     '(lambda () (interactive) (other-frame -1))
    "g a" 'describe-char
    "g o" 'ff-find-other-file
    "g O" 'ff-find-other-file-other-window)
  ;; "g a"    'describe-char)
  ;; (:keymaps 'inner
  ;;  "e"      'my-evil-a-buffer)
  ;; (:keymaps 'outer
  ;;  "e"      'my-evil-a-buffer)
  :custom
  ;; (evil-undo-system (if (featurep 'undo-tree) 'undo-tree 'undo-redo))
  ;; (evil-undo-system (cond
  ;;                    ((featurep 'undo-tree) 'undo-tree)
  ;;                    ((>= emacs-major-version 28) 'undo-redo)
  ;;                    (t nil)))
  (evil-undo-system 'undo-tree)
  ;; (evil-undo-system (if (>= 28 emacs-major-version)
  ;;                       'undo-redo
  ;;                     (require 'undo-fu)
  ;;                     'undo-fu))
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
    ;; nil
    "`set splitbelow` in vim")
  (evil-vsplit-window-right
    t
    ;; nil
    "`set splitright` in vim")
  (evil-move-beyond-eol
    t
    "As recommended by evil-cleverparens")
  (evil-auto-indent
    t
    "Automatically indent when inserting a newline")
  (evil-want-fine-undo t)
  ;; this messes up subtle bits of some emacs modes, like ediff
  :hook (;; (window-configuration-change-hook . balance-windows)
          (evil-normal-state-entry-hook . evil-ex-nohighlight))
  :config
  (defun update-evil-shift-width ()
    "We do this otherwise packages like parinfer would mess up with
      the indentation, since their default is 4 but lisp-mode defaults
      are generally 2."
    (require 'evil)
    (customize-set-variable 'evil-shift-width lisp-body-indent))

  (define-advice evil-ex-define-cmd
      (:before-while (_name cmd) --does-cmd-have-interactive-clause)
    (unless (commandp cmd)
      (warn "evil-ex-define-cmd only accepts commands. Are you missing an `interactive' form?"))
    (commandp cmd))

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
  (my-evil-define-split-vsplit-cmd "var[iables]" 'find-user-variables-file)
  (evil-ex-define-cmd "Tvar[iables]" #'(lambda () (interactive)
                                          (my-new-cmd-tab user-variables-file)))
  (my-evil-define-split-vsplit-cmd "buffers" 'ibuffer)
  (my-evil-define-split-vsplit-cmd "me[ssage]"
                                    #'(lambda ()
                                        (switch-to-buffer "*Messages*")))
  ;; (my-evil-define-split-vsplit-cmd "sc[ratch]"
  ;;                                  #'(lambda ()
  ;;                                      (switch-to-buffer "*scratch*")))

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

  ;; (evil-define-text-object my-evil-a-buffer (count &optional beg end type)
  ;;   "Select entire buffer"
  ;;   (evil-range (point-min) (point-max)))

  (evil-mode))

;;; evil-collection

(use-package evil-collection
  ;;    :straight (:host github :repo "emacs-evil/evil-collection"
  ;;               :files (:defaults ("modes" "modes/*")))
  :custom
  (evil-collection-setup-minibuffer t)
  ;; the following causes a crash because:
  ;; Lisp error: (void-variable org-agenda-diary-file)
  ;; (evil-collection-calendar-want-org-bindings t)
  :config
  (evil-collection-init))

(provide 'config::evil)

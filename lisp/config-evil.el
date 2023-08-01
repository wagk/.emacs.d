(require 'use-package)
(require 'general)

(customize-set-value 'evil-want-keybinding nil
                     "`evil-collections' wants this to be disabled
                     before even loading evil, see
                     https://github.com/emacs-evil/evil-collection/issues/60")

;; NOTE: reddit notes that there might still be some history
;; corruption?
(use-package undo-tree
  :straight t
  :demand t
  :commands (turn-on-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-auto-save-history nil) ;; the perf of this seems intensely bad
  (undo-tree-enable-undo-in-region t)
  :hook
  (evil-local-mode-hook . turn-on-undo-tree-mode)
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
  :commands (evil-set-initial-state
             evil-insert-state
             evil-ex-define-cmd)
  :general
  (:states '(normal visual)
    :prefix my-default-evil-leader-key
    "<SPC>" 'execute-extended-command)
  (:keymaps 'visual
    "J" 'evil-join)
  (:keymaps 'insert
    "C-l" 'evil-complete-next-line
    "C-u" 'evil-delete-whole-line)
  (:keymaps 'normal
    "g C-u" 'universal-argument
    "g a" 'describe-char
    "g o" 'ff-find-other-file
    "g O" 'ff-find-other-file-other-window)
  :custom
  ;; (evil-complete-next-func
  ;;  #'(lambda (_arg) (completion-at-point))
  ;;    "Note that this means the variable `evil-complete-all-buffers'
  ;;    will be ignored when `C-n' is pressed, since this was where the
  ;;    logic was originally defined.")
  ;; (evil-complete-previous-func
  ;;  #'evil-complete-next-func
  ;;    "Refer to `evil-complete-next-func' for warnings on how this
  ;;    impacts evil configuration variables.")
  (evil-undo-system 'undo-tree)
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
  (evil-want-fine-undo t)
  :hook ((evil-normal-state-entry-hook . evil-ex-nohighlight))
  :config
  (defun update-evil-shift-width ()
    "We do this otherwise packages like parinfer would mess up with
      the indentation, since their default is 4 but lisp-mode defaults
      are generally 2."
    (require 'evil)
    (customize-set-variable 'evil-shift-width lisp-body-indent))

  ;; Back to our regularly scheduled programming
  (evil-select-search-module 'evil-search-module 'evil-search)

  (evil-ex-define-cmd "bc[lose]" 'kill-this-buffer)
  (evil-define-command my-evil-vsplit-buffer (&optional buffer)
    "Strictly speaking this isn't implemented in vim, which is why
    we're adding a custom function for it here."
    :repeat nil
    (interactive "<b>")
    (evil-window-vsplit)
    (evil-buffer buffer))

  (evil-ex-define-cmd "vb[uffer]" 'my-evil-vsplit-buffer)

  (evil-mode))

;;; evil-collection

(use-package evil-collection
  :straight t
  :custom
  (evil-collection-setup-minibuffer t)
  ;; the following causes a crash because:
  ;; Lisp error: (void-variable org-agenda-diary-file)
  ;; (evil-collection-calendar-want-org-bindings t)
  :config
  (evil-collection-init))

(use-package evil-lion
  :straight (:host github :repo "edkolev/evil-lion")
  :after (evil)
  :general
  (:keymaps '(normal visual)
   "gl"     'evil-lion-left
   "gL"     'evil-lion-right))

;; :reverse           reverse visually selected lines
;; :remove            remove current file and its buffer
;; :rename NEW-PATH   rename or move current file and its buffer
;; :colorscheme THEME change emacs color theme
;; :diff-orig         get a diff of unsaved changes, like vim's common :DiffOrig
;; :gdiff             BRANCH git-diff current file, requires magit and vdiff-magit
;; :gblame            git-blame current file, requires magit
;; :gremove           git remove current file, requires magit
;; :tyank             copy range into tmux paste buffer, requires running under tmux
;; :tput              paste from tmux paste nuffer, requires running under tmux
(use-package evil-expat
  :straight (:host github :repo "edkolev/evil-expat"))

(use-package evil-matchit
  :straight (:host github :repo "redguardtoo/evil-matchit")
  :after evil
  :config
  (global-evil-matchit-mode))

(use-package evil-visualstar
  :straight (:host github :repo "bling/evil-visualstar")
  :general
  (:keymaps 'visual
   "*" 'evil-visualstar/begin-search-forward
   "#" 'evil-visualstar/begin-search-backward))

(general-define-key
 :keymaps 'global
 "C-\\" 'toggle-input-method)

(use-package evil-surround
  :straight (:host github :repo "emacs-evil/evil-surround")
  :after (evil)
  :config
  (global-evil-surround-mode))

(use-package evil-embrace
  :straight t
  :custom
  (evil-embrace-show-help-p
   nil
   "Otherwise it shows a which-key like interface (that I'm not so hot on)")
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-args
  :straight (:host github :repo "wcsmith/evil-args")
  :after (evil)
  :general
  (evil-inner-text-objects-map
   "a" 'evil-inner-arg)
  (evil-outer-text-objects-map
   "a" 'evil-outer-arg)
  :config
  ;; these variables don't exist until evil-arg loads
  (push "<" evil-args-openers)
  (push ">" evil-args-closers))

(use-package evil-commentary
  :straight (:host github :repo "linktohack/evil-commentary")
  :after (evil)
  :config
  (evil-commentary-mode))

(use-package evil-indent-plus
  :straight (:host github :repo "TheBB/evil-indent-plus")
  :general
  (evil-inner-text-objects-map
   "i" 'evil-indent-plus-i-indent
   "I" 'evil-indent-plus-a-indent)
  (evil-outer-text-objects-map
   "i" 'evil-indent-plus-i-indent-up
   "I" 'evil-indent-plus-a-indent-up))

(provide 'config-evil)

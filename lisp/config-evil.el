(require 'use-package)

(customize-set-value 'evil-want-keybinding nil
                     "`evil-collections' wants this to be disabled
                     before even loading evil, see
                     https://github.com/emacs-evil/evil-collection/issues/60")

;; NOTE: reddit notes that there might still be some history
;; corruption?
(use-package undo-tree
  :ensure (:host github :repo "emacs-straight/undo-tree" :branch "master")
  :demand t
  :blackout t
  :after (evil general config-theme)
  :commands (turn-on-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  ;; (undo-tree-auto-save-history nil) ;; the perf of this seems intensely bad
  (undo-tree-history-directory-alist
   `(("." . ,(expand-file-name (file-name-concat user-emacs-directory "undo-tree")))))
  (undo-tree-enable-undo-in-region t)
  :custom-face
  (undo-tree-visualizer-default-face
   ((default . (:foreground unspecified
                :inherit sol-foreground))))
  (undo-tree-visualizer-active-branch-face
   ((default . (:foreground ,sol-green))))
  (undo-tree-visualizer-current-face
   ((default . (:inherit sol-superlight-background))))
  (undo-tree-visualizer-register-face
   ((default . (:foreground ,sol-yellow))))
  (undo-tree-visualizer-unmodified-face
   ((default . (:foreground ,sol-blue))))
  :hook
  (evil-local-mode-hook . turn-on-undo-tree-mode)
  :general
  (:states 'normal
   "U"     'undo-tree-visualize)
  :config
  (require 'track-changes nil :noerror) ;; if present, load track-changes
  (global-undo-tree-mode)
  (with-eval-after-load 'evil
    (setq evil-undo-system 'undo-tree))
  (with-eval-after-load 'no-littering
    (setq undo-tree-history-directory-alist
     `(("." . ,(file-name-concat no-littering-etc-directory "undo-tree"))))))

;; Needed for g; and g,
(use-package goto-chg)

;;; Evil-mode

(cl-defun --evil-do-in-split (func &optional split-type)
  "Call FUNC after splitting by SPLIT-TYPE.
SPLIT-TYPE must be either `:split' or `:vsplit'"
  (interactive)
  (let ((orig-window-config (current-window-configuration)))
    (pcase split-type
      (:split (evil-window-split))
      (:vsplit (evil-window-vsplit)))
    (condition-case err
        (command-execute func)
      (t (delete-window)
         (set-window-configuration orig-window-config)
         (message "%s" err)))))

(cl-defun --evil-do-in-tab (func)
  "Call FUNC in a new tab."
  (interactive)
  (require 'tab-bar)
  (tab-bar-new-tab)
  (condition-case err
      (command-execute func)
    (t (tab-bar-close-tab)
       (message "%s" err))))

(cl-defun --evil-do-in-frame (func)
  "Call FUNC in a new frame."
  (interactive)
  (make-frame)
  (condition-case err
       (command-execute func)
    (t (delete-frame)
       (message "%s" err))))

(cl-defun --evil-smart-split (func)
  "Call FUNC after splitting along longest axis."
  (interactive)
  (let ((style (if (< (window-pixel-height)
                      (window-pixel-width))
                   :vsplit :split)))
    (--evil-do-in-split func style)))

(use-package evil
  :demand t
  :after (general config-theme undo-tree goto-chg)
  :ensure (:host github :repo "emacs-evil/evil")
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
    "g r" #'(lambda () (interactive) (revert-buffer t t))
    "g a" 'describe-char
    "g o" 'ff-find-other-file
    "g O" 'ff-find-other-file-other-window)
  (evil-window-map
   "]"   '--evil-window-tag
   "C-]" '--evil-window-tag)
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
  (evil-v$-excludes-newline t)
  (evil-want-C-u-scroll
    t
    "In vim, <C-u> maps to half page up. In Emacs, it corresponds to
    the `universal-argument' function that might augment a function
    call. We prefer the scrolling.")
  (evil-want-C-u-delete
    t
    "Apparently C-u deletes back to indentation in insert state.")
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
  :custom-face
  (evil-ex-search ((default . (:inherit match))))
  (evil-ex-info ((default . (:foreground ,sol-red))))
  (evil-ex-substitute-replacement ((default . (:foreground ,sol-red))))
  :hook ((evil-normal-state-entry-hook . evil-ex-nohighlight)
         (after-init-hook . --update-evil-shift-width-for-emacs-lisp))
  :init
  (cl-defun update-evil-shift-width ()
    "We do this otherwise packages like parinfer would mess up with
      the indentation, since their default is 4 but lisp-mode defaults
      are generally 2."
    (require 'evil)
    (customize-set-variable 'evil-shift-width lisp-body-indent))
  (cl-defun --update-evil-shift-width-for-emacs-lisp ()
    (add-hook 'emacs-lisp-mode-hook #'update-evil-shift-width))
  :config
  (cl-defun --evil-window-tag ()
    ":h window-tag"
    (interactive)
    (--evil-do-in-split #'evil-jump-to-tag :vsplit))
  ;; Back to our regularly scheduled programming
  (evil-select-search-module 'evil-search-module 'evil-search)

  (evil-ex-define-cmd "bc[lose]" #'(lambda () (interactive)
                                     (kill-buffer (current-buffer))))
  (evil-define-command my-evil-vsplit-buffer (&optional buffer)
    "Strictly speaking this isn't implemented in vim, which is why
    we're adding a custom function for it here."
    :repeat nil
    (interactive "<b>")
    (evil-window-vsplit)
    (evil-buffer buffer))

  (evil-ex-define-cmd "E" "edit")

  (with-eval-after-load 'link-hint
    ;; (cl-defun --link-hint-browse-point-or-open-link ()
    ;;   (interactive)
    ;;   (condition-case err
    ;;       (browse-url-at-point)
    ;;     (t (link-hint-open-link)))
    (general-define-key
     :states 'normal
      "g x" #'link-hint-open-link-at-point
      "g X" #'link-hint-copy-link-at-point))
      ;; "g x" #'--link-hint-browse-point-or-open-link))

  (evil-ex-define-cmd "vb[uffer]" 'my-evil-vsplit-buffer)

  (defcustom --evil-write-typo-filenames '("'" "[" "]" "\"")
    "Filenames that commonly get created when I mash `:w'.")

  (define-advice evil-fill (:after (&rest args) --move-cursor-to-end-column)
    (evil-scroll-end-column))

  (define-advice evil-write (:before-while (&rest args) --reject-typo-filenames)
    "When calling :w I want to reject weird typo filenames"
    (let ((save-name (nth 3 args)))
      (if-let* ((save (-any (lambda (x) (equal x save-name))
                            --evil-write-typo-filenames)))
          (y-or-n-p
           (format "Did you intend to save a file named %s ?" save-name))
        t)))

  (evil-mode))

;;; evil-collection

(use-package evil-collection
  :after (evil)
  :blackout (evil-collection-unimpaired-mode)
  :custom
  (evil-collection-setup-minibuffer t)
  ;; the following causes a crash because:
  ;; Lisp error: (void-variable org-agenda-diary-file)
  ;; (evil-collection-calendar-want-org-bindings t)
  (evil-collection-unimpaired-want-repeat-mode-integration t)
  :config
  (evil-collection-init))

(use-package evil-lion
  :ensure (:host github :repo "edkolev/evil-lion")
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
  :disabled t
  :ensure (:host github :repo "edkolev/evil-expat"))

(use-package evil-matchit
  :ensure (:host github :repo "redguardtoo/evil-matchit")
  :after evil
  :config
  (global-evil-matchit-mode))

(use-package evil-visualstar
  :ensure (:host github :repo "bling/evil-visualstar")
  :after (evil general)
  :general
  (:keymaps 'visual
   "*" 'evil-visualstar/begin-search-forward
   "#" 'evil-visualstar/begin-search-backward))

(with-eval-after-load 'general
  (general-define-key
   :keymaps 'global
   "C-\\" 'toggle-input-method))

(use-package evil-surround
  :demand t
  :ensure (:host github :repo "emacs-evil/evil-surround")
  :after (evil)
  :config
  (global-evil-surround-mode))

(use-package evil-embrace
  :disabled t
  :after (embrace evil evil-surround)
  :custom
  (evil-embrace-show-help-p
   nil
   "Otherwise it shows a which-key like interface (that I'm not so hot on)")
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-args
  :ensure (:host github :repo "wcsmith/evil-args")
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
  :ensure (:host github :repo "linktohack/evil-commentary")
  :blackout t
  :general
  (evil-commentary-mode-map
   "s-/" nil)
  :after (evil)
  :config
  (evil-commentary-mode))

(use-package evil-indent-plus
  :after evil
  :ensure (:host github :repo "TheBB/evil-indent-plus")
  :general
  (evil-inner-text-objects-map
   "i" 'evil-indent-plus-i-indent
   "I" 'evil-indent-plus-a-indent)
  (evil-outer-text-objects-map
   "i" 'evil-indent-plus-i-indent-up
   "I" 'evil-indent-plus-a-indent-up))

(with-eval-after-load 'evil
  (require 'config-evil-helpers))

(use-package evil-textobj-tree-sitter
  :ensure (:host github
           :repo "meain/evil-textobj-tree-sitter"
           :files (:defaults "queries"))
  :after (evil general)
  :general
  (evil-outer-text-objects-map
   "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (evil-inner-text-objects-map
   "f" (evil-textobj-tree-sitter-get-textobj "function.inner")))

(provide 'config-evil)

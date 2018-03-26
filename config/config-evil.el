;;; config-evil.el --- evil-mode configuration

;;; Commentary:

;;; Code:
(require 'config-package)

;; activate folding
(add-hook 'prog-mode-hook 'hs-minor-mode)
;; (evil-mode)

;; Note that all the evil flags are documented in `evil.info' in the evil
;; directory
(use-package evil
  :demand t
  :general
  (:keymaps 'insert
   "C-u"    'kill-whole-line
   "C-l"    'evil-complete-next-line
   "C-L"    'evil-complete-previous-line
   "C-p"    'evil-complete-next
   "C-n"    'evil-complete-previous
   "C-k"    nil)
  (:keymaps 'motion
   "C-u"    'evil-scroll-up)
  (:keymaps 'normal
   "Y"      '/evil-copy-to-end-of-line
   "gt"     '/evil-gt
   "gT"     '/evil-gT
   "C-\\"   '/lang-toggle ;; binding for eng <-> jap
   "g o"    'ff-find-other-file)
  (:keymaps 'visual
   ">>"     '/evil-shift-right-visual
   "<<"     '/evil-shift-left-visual)
  (:keymaps 'inner
   "/"      '/inner-forward-slash
   "l"      'my-evil-inner-line)
  (:keymaps 'outer
   "e"      'my-evil-a-buffer
   "l"      'my-evil-a-line
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
  :config

  ;; TODO: figure out this
  ;; https://github.com/syl20bnr/spacemacs/issues/5070
;;;###autoload
  (defun /evil-paste-after-from-0 ()
    "I legitimately forgot what this does.
Probably copied it from stackoverflow"
    (interactive)
    (let ((evil-this-register ?0))
      (call-interactively 'evil-paste-after)))

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

;;;###autoload
  (defun /lang-toggle ()
    "Input language toggle wrapper."
    (interactive)
    (toggle-input-method)
    ;; (evil-append 1)
    )

  ;; Overload shifts so that they don't lose the selection
;;;###autoload
  (defun /evil-shift-left-visual ()
    "Keep visual selection after shifting left."
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

;;;###autoload
  (defun /evil-shift-right-visual ()
    "Same as /evil-shift-left-visual, but for the right instead."
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

;;;###autoload
 (defun evil-unimpaired//find-relative-filename (offset)
    (when buffer-file-name
      (let* ((directory (f-dirname buffer-file-name))
             (files (f--files directory (not (s-matches? "^\\.?#" it))))
             (index (+ (-elem-index buffer-file-name files) offset))
             (file (and (>= index 0) (nth index files))))
        (when file
          (f-expand file directory)))))

;;;###autoload
  (defun evil-unimpaired/previous-file ()
    (interactive)
    (-if-let (filename (evil-unimpaired//find-relative-filename -1))
        (find-file filename)
      (user-error "No previous file")))

;;;###autoload
  (defun evil-unimpaired/next-file ()
    (interactive)
    (-if-let (filename (evil-unimpaired//find-relative-filename 1))
        (find-file filename)
      (user-error "No next file")))

;;;###autoload
  (defun evil-unimpaired/paste-above ()
    (interactive)
    (evil-insert-newline-above)
    (evil-paste-after 1))

;;;###autoload
  (defun evil-unimpaired/paste-below ()
    (interactive)
    (evil-insert-newline-below)
    (evil-paste-after 1))

;;;###autoload
  (defun evil-unimpaired/insert-space-above (count)
    (interactive "p")
    (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

;;;###autoload
  (defun evil-unimpaired/insert-space-below (count)
    (interactive "p")
    (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

;;;###autoload
  (defun evil-unimpaired/next-frame ()
    (interactive)
    (/evil-gt))

;;;###autoload
  (defun evil-unimpaired/previous-frame ()
    (interactive)
    (/evil-gT))

  ;; from tpope's unimpaired
  (define-key evil-normal-state-map (kbd "[ SPC")
    'evil-unimpaired/insert-space-above)
  (define-key evil-normal-state-map (kbd "] SPC")
    'evil-unimpaired/insert-space-below)
  ;; (define-key evil-normal-state-map (kbd "[ e") 'move-text-up)
  ;; (define-key evil-normal-state-map (kbd "] e") 'move-text-down)
  (define-key evil-visual-state-map (kbd "[ e") ":move'<--1")
  (define-key evil-visual-state-map (kbd "] e") ":move'>+1")
  ;; (define-key evil-visual-state-map (kbd "[ e") 'move-text-up)
  ;; (define-key evil-visual-state-map (kbd "] e") 'move-text-down)
  (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "] b") 'next-buffer)
  (define-key evil-normal-state-map (kbd "[ f") 'evil-unimpaired/previous-file)
  (define-key evil-normal-state-map (kbd "] f") 'evil-unimpaired/next-file)
  ;; (define-key evil-normal-state-map (kbd "[ t") 'evil-unimpaired/previous-frame)
  ;; (define-key evil-normal-state-map (kbd "] t") 'evil-unimpaired/next-frame)
  (define-key evil-normal-state-map (kbd "[ w") 'previous-multiframe-window)
  (define-key evil-normal-state-map (kbd "] w") 'next-multiframe-window)
  ;; select pasted text
  (define-key evil-normal-state-map (kbd "g p") (kbd "` [ v ` ]"))
  ;; paste above or below with newline
  (define-key evil-normal-state-map (kbd "[ p") 'evil-unimpaired/paste-above)
  (define-key evil-normal-state-map (kbd "] p") 'evil-unimpaired/paste-below)

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

  ;; (add-hook 'view-mode-hook 'evil-motion-state)

  ;; (evil-define-text-object /a-forward-slash (count &optional beg end type)
  ;;   "Select forward slash (/)"
  ;;   :extend-selection t
  ;;   (evil-select-quote ?/ beg end type count))

  ;; (evil-define-text-object /inner-forward-slash (count &optional beg end type)
  ;;   "Select forward slash (/)"
  ;;   :extend-selection nil
  ;;   (evil-select-quote ?/ beg end type count))

  ;; ;; Let `_` be considered part of a word, like vim does
  ;; (defadvice evil-inner-word (around underscore-as-word activate)
  ;;   (let ((table (copy-syntax-table (syntax-table))))
  ;;     (modify-syntax-entry ?_ "w" table)
  ;;     (with-syntax-table table ad-do-it)))
  (/treat-underscore-as-word) ;TODO: Not sure if this is required if we're hooking into prog-mode

  ;; (defun my-evil-make-frame-with-params (file)
  ;;   "Tries to emulate evil tab creation using `make-frame'"
  ;;   (interactive "<f>")
  ;;   (if file
  ;;       ;; Finds the file and loads it into the frame
  ;;       )
  ;;   )

  ;; (evil-ex-define-cmd "sh[ell]"   'eshell)
  (evil-ex-define-cmd "sh[ell]"   'shell) ;; at least shell shows its keymaps
  (evil-ex-define-cmd "tabn[ew]"  'make-frame)
  (evil-ex-define-cmd "tabe[dit]" 'make-frame)
  (evil-ex-define-cmd "restart"   'restart-emacs)
  (evil-ex-define-cmd "init"      'find-user-init-file)

  ;; (lexical-let ((default-color (cons (face-background 'mode-line)
  ;;                                    (face-foreground 'mode-line))))
  ;;   (add-hook 'post-command-hook
  ;;             (lambda ()
  ;;               (let ((color (cond ((minibufferp) default-color)
  ;;                                  ((evil-insert-state-p) '("#b58900" . "#ffffff"))
  ;;                                  ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
  ;;                                  ((buffer-modified-p)   '("#dc322f" . "#ffffff"))
  ;;                                  (t default-color))))
  ;;                 (set-face-background 'mode-line (car color))
  ;;                 (set-face-foreground 'mode-line (cdr color))))))

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
  ;; (/evil-define-and-bind-text-object "l" "^\\s-*" "\\s-*$") ;; line textobj
  ;; (/evil-define-and-bind-text-object "e" "\\`\\s-*" "\\s-*$") ;; buffer textobj

  (evil-define-text-object my-evil-a-buffer (count &optional beg end type)
    "Select entire buffer"
    (evil-range (point-min) (point-max)))

  ;; shamelessly stolen from
  ;; https://github.com/syohex/evil-textobj-line/blob/master/evil-textobj-line.el
  (defun my-evil-line-range (count beg end type &optional inclusive)
    (if inclusive
        (evil-range (line-beginning-position) (line-end-position))
      (let ((start (save-excursion
                     (back-to-indentation)
                     (point)))
            (end (save-excursion
                   (goto-char (line-end-position))
                   (skip-syntax-backward " " (line-beginning-position))
                   (point))))
        (evil-range start end))))

  (evil-define-text-object my-evil-a-line (count &optional beg end type)
    "Select entire line"
    (my-evil-line-range count beg end type t))

  (evil-define-text-object my-evil-inner-line (count &optional beg end type)
    "Select an inner line"
    (my-evil-line-range count beg end type))

  (add-hook 'evil-normal-state-entry-hook 'evil-ex-nohighlight)
  (evil-mode)
  )

(use-package evil-collection
  :after (evil)
  :demand t
  :config
  ;;NOTE: note that this REQUIRES the var `evil-want-integration' to be NIL
  (evil-collection-init))

;; defaults to g~
(use-package evil-string-inflection
  :disabled
  :after (evil))

;; defaults to x, so dax, dix, etc
;; This package is about xml attribute objects, the t textobj handles tags, not
;; attributes, which are inside tags
(use-package exato
  :after (evil))

;; https://github.com/gridaphobe/evil-god-state
(use-package evil-god-state
  :general
  (:states 'normal
   "g <SPC>" 'evil-execute-in-god-state))

;;; TODO: Org-mode has some pairs that are not handled by surround. We would
;;; need to fix that.
(use-package evil-surround
  :after (evil)
  :demand t
  :config
  (global-evil-surround-mode)
  )

;;; Evil-embrace is like a souped up addon of surround, this time they have
;;; things like function surround and probably more features.
(use-package evil-embrace
  :after (evil-surround)
  :demand t
  :config
  (evil-embrace-enable-evil-surround-integration)
  (setq evil-embrace-show-help-p nil)
  )

(use-package evil-args
  :bind (:map evil-inner-text-objects-map
         ("a" . evil-inner-arg)
         :map evil-outer-text-objects-map
         ("a" . evil-outer-arg)
         ;; :map evil-normal-state-map
         ;; ("L" . evil-forward-arg)
         ;; ("H" . evil-backward-arg)
         ;; ("K" . evil-jump-out-args)
         ;; :map evil-motion-state-map
         ;; ("L" . evil-forward-arg)
         ;; ("H" . evil-backward-arg)
         )
  ;; :config
  ;; consider spaces as argument delimiters
  ;; (add-to-list 'evil-args-delimiters " ")
  )

;; more like evil-textobj-kolumn
(use-package evil-textobj-column
  :bind (:map evil-inner-text-objects-map
         ("k" . evil-textobj-column-word)
         ("K" . evil-textobj-column-WORD)))

(use-package evil-numbers
  :general
  (:keymaps 'normal
   "C-a"  'evil-numbers/inc-at-pt
   "C-x"  'evil-numbers/dec-at-pt)
  ;; :bind (:map evil-normal-state-map
  ;;        ("C-a" . evil-numbers/inc-at-pt)
  ;;        ("C-x" . evil-numbers/dec-at-pt))
  )

(use-package evil-rsi
  :disabled
  :after (evil)
  :config
  (evil-rsi-mode))

;; alignment
(use-package evil-lion
  :after (evil)
  :demand t
  :config
  (evil-lion-mode))

(use-package evil-matchit)

;; (use-package evil-paredit
;;   :config (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode))

;; (use-package evil-cleverparens
;;   :ensure t
;;   :bind(:map evil-inner-text-objects-map
;;              ("c" . evil-cp-inner-comment)
;;              :map evil-outer-text-objects-map
;;              ("c" . evil-cp-a-comment)
;;              )
;;   :config
;;   ;; (progn (require 'evil-cleverparens-text-objects)
;;   ;;        (define-key evil-inner-text-objects-map "c" 'evil-cp-inner-comment)
;;   ;;        (define-key evil-outer-text-objects-map "c" 'evil-cp-a-comment))
;;   (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode))

;; (use-package evil-cleverparens-text-objects
;;   :ensure t
;;   :init
;;   (use-package evil-cleverparens :ensure t)
;;   :config
;;   nil)

;; (use-package evil-cleverparens
;;   :bind(:map evil-inner-text-objects-map
;;              ("c" . evil-cp-inner-comment)
;;              :map evil-outer-text-objects-map
;;              ("c" . evil-cp-a-comment))
;;   ;; :config
;;   ;; (require 'evil-cleverparens-text-objects)
;;   )

;; Adds textobjects that comments
(use-package evil-commentary
  :after (evil)
  :demand t
  :config
  (evil-commentary-mode)
  )

(use-package evil-nerd-commenter
  :after (evil)
  :bind (:map evil-inner-text-objects-map
         ("c" . evilnc-inner-comment)
         :map evil-outer-text-objects-map
         ("c" . evilnc-outer-commenter)))

;; (use-package evil-replace-with-register)

;; (use-package evil-text-object-python)

;;; Indentation text object for evil
(use-package evil-indent-plus
  :bind(:map evil-inner-text-objects-map
        ("i" . evil-indent-plus-i-indent)
        ("I" . evil-indent-plus-a-indent)
        :map evil-outer-text-objects-map
        ("i" . evil-indent-plus-i-indent-up)
        ("I" . evil-indent-plus-a-indent-up)))

;; vim A E S T H E T H I C S
;; Puts tildes in the fringe, just like vim.
(use-package vi-tilde-fringe
  :after (evil)
  :demand t
  :config
  (global-vi-tilde-fringe-mode))

;;; Allows for * and # commands. which originally only worked on WORDs, to
;;; work on a visual selection too
(use-package evil-visualstar
  :after (evil)
  :demand t
  :config
  (global-evil-visualstar-mode))

;; TODO: Document GNU Readline bindings
(use-package evil-rsi
  :demand t
  :after (evil)
  :diminish (evil-rsi-mode)
  :config
  (evil-rsi-mode))

;; Flashes the selection you made. I honestly don't need this and am just
;; turning it on for shits and giggles, until it starts to annoy me
;; https://github.com/edkolev/evil-goggles
(use-package evil-goggles
  :after (evil)
  :diminish (evil-goggles-mode)
  :demand t
  :custom
  (evil-goggles-duration 0.05
                         "Sometimes the default of 0.2 is too slow")
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;;; Disabled because it conflicts with evil-snipe-override-mode
(use-package evil-quickscope
  :disabled t
  ;; :config
  ;; (global-evil-quickscope-always-mode t)
  ;; (global-evil-quickscope-mode t)
  )

;;; Basically does what Clever-F did in vim, letting you repeatedly press
;;; f, F, t, and T instead of using ; and ,
(use-package evil-snipe
  :after (evil)
  :demand t
  :diminish (evil-snipe-override-mode
             evil-snipe-override-local-mode)
  :config
  (evil-snipe-override-mode))

;;; Adds the following ex commands:
;; | :reverse           | reverse visually selected lines
;; | :remove            | remove current file and its buffer
;; | :rename NEW-PATH   | rename or move current file and its buffer
;; | :colorscheme THEME | change emacs color theme
;; | :diff-orig         | get a diff of unsaved changes, like vim's common :DiffOrig
;; | :gdiff             | BRANCH git-diff current file, requires magit and vdiff-magit
;; | :gblame            | git-blame current file, requires magit
;; | :gremove           | git remove current file, requires magit
;; | :tyank             | copy range into tmux paste buffer, requires running under tmux
;; | :tput              | paste from tmux paste buffer, requires running under tmux
(use-package evil-expat)

;;; Adds an operator `gx' that, when called again, swaps both selections
;; currently DISABLED because it conflicts with the default `g x', which
;; goes to the link under the cursor (`browse-url-at-point'), something
;; which I feel is probably cooler than evil-exchange
(use-package evil-exchange
  :disabled t)

;; (use-package evil-visual-mark-mode
;;   :ensure t
;;   :config
;;   (evil-visual-mark-mode))

;; (use-package evil-tabs
;;   :ensure t
;;   :config
;;   (global-evil-tabs-mode t))

(use-package vimish-fold)

(use-package evil-tutor)

(provide 'config-evil)

;;; config-evil.el ends here

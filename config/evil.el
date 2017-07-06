;;; evil.el --- evil-mode configuration

;;; Commentary:
;; requires use-package

;;; Code:

;; local functions
(defun my-evil-gt () (interactive)
  (other-frame 1))

(defun my-evil-gT () (interactive)
  (other-frame -1))

(defun my-lang-toggle () (interactive)
  (toggle-input-method)
  (evil-append 1))

(use-package evil-leader
             :ensure t
             :config
             (global-evil-leader-mode)
             (evil-leader/set-leader "<SPC>"))

(use-package evil
             :ensure t
             :bind (("C-f" . universal-argument)
                    ("C-u" . kill-whole-line)
                    :map universal-argument-map
                    ("C-u" . nil)
                    ("C-f" . universal-argument-more)
                    :map evil-motion-state-map
                    ("C-u" . evil-scroll-up)
                    :map evil-normal-state-map
                    ("gt" . my-evil-gt)
                    ("gT" . my-evil-gT)
                    ("C-\\" . my-lang-toggle)
                    :map evil-visual-state-map
                    ("p" . evil-paste-after-from-0)) ;; NOTE: function defined *below*. Check if this loads

             :init
             (setq evil-want-C-u-scroll t)

             :config
             (fset 'evil-visual-update-x-selection 'ignore)
             (setq evil-want-Y-yank-to-eol t
                   sentence-end-double-space nil
                   evil-regexp-search t
                   ; evil-normal-state-cursor '(box "red"))
                   evil-normal-state-modes (append evil-motion-state-modes
                                                   evil-normal-state-modes)
                   evil-motion-state-modes nil)
             (add-hook 'view-mode-hook 'evil-motion-state)

             ;; Let `_` be considered part of a word, like vim does
             (defadvice evil-inner-word (around underscore-as-word activate)
                        (let ((table (copy-syntax-table (syntax-table))))
                          (modify-syntax-entry ?_ "w" table)
                          (with-syntax-table table ad-do-it)))

             ;; TODO: figure out this
             (defun evil-paste-after-from-0 ()
               (interactive)
               (let ((evil-this-register ?0))
                 (call-interactively 'evil-paste-after)))

             (evil-ex-define-cmd "tabn[ew]" 'make-frame)
             (evil-ex-define-cmd "vsp[lit]" #'(lambda() (interactive)
                                                       (split-window-horizontally)
                                                       (other-window 1)))
             (evil-ex-define-cmd "sp[lit]" #'(lambda() (interactive)
                                                      (split-window-vertically)
                                                      (other-window 1)))

             (lexical-let ((default-color (cons (face-background 'mode-line)
                                                (face-foreground 'mode-line))))
                          (add-hook 'post-command-hook
                                    (lambda ()
                                      (let ((color (cond ((minibufferp) default-color)
                                                         ((evil-insert-state-p) '("#b58900" . "#ffffff"))
                                                         ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                                         ((buffer-modified-p)   '("#dc322f" . "#ffffff"))
                                                         (t default-color))))
                                        (set-face-background 'mode-line (car color))
                                        (set-face-foreground 'mode-line (cdr color)))))))

(use-package evil-surround
             :ensure t
             :config
             (global-evil-surround-mode 1)
             (setq-default evil-surround-pairs-alist (cons '(?~ . ("~" . "~"))
                                                           evil-surround-pairs-alist)))

(use-package evil-args
             :ensure t
             :bind (:map evil-inner-text-objects-map
                    ("i" . evil-inner-arg)
                    :map evil-outer-text-objects-map
                    ("a" . evil-outer-arg)
                    :map evil-normal-state-map
                    ("L" . evil-forward-arg)
                    ("H" . evil-backward-arg)
                    ("K" . evil-jump-out-args)
                    :map evil-motion-state-map
                    ("L" . evil-forward-arg)
                    ("H" . evil-backward-arg)))

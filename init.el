(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "04886f04f33dfc156dc60b5a4b1f6f67da710eae8b39af6cf72f5f0b908948ee" default)))
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (evil-text-object-python tramp-term company powershell use-package solarized-theme org-evil helm evil-surround evil-replace-with-register evil-numbers evil-magit evil-leader evil-commentary evil-args)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit fixed-pitch :foreground "#cb4b16" :height 1.3))))
 '(org-level-2 ((t (:inherit fixed-pitch :foreground "#859900" :height 1.2))))
 '(org-level-3 ((t (:inherit fixed-pitch :foreground "#268bd2" :height 1.15))))
 '(org-level-4 ((t (:inherit fixed-pitch :foreground "#b58900" :height 1.1))))
 '(org-level-5 ((t (:inherit fixed-pitch :foreground "#2aa198"))))
 '(org-level-6 ((t (:inherit fixed-pitch :foreground "#859900"))))
 '(org-level-7 ((t (:inherit fixed-pitch :foreground "#dc322f"))))
 '(org-level-8 ((t (:inherit fixed-pitch :foreground "#268bd2")))))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             '("melpa-stable" . "http://stable.melpa.org/packages/")) ;; what is the trailing t for?
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; make sure we download when necessary
(setq use-package-always-ensure t)

;; turn on line numbers
(global-linum-mode 1)

;; startup maximised
(add-to-list 'default-frame-alist '(fullscreen . maximised))

;; autopairing
(electric-pair-mode 1)

; Remove toolbar
(tool-bar-mode -1)

;; set default font
(add-to-list 'default-frame-alist '(font . "Consolas-11"))

;;datetime things
(defvar current-date-time-format "%Y-%m-%dT%H:%M:%S"
  "Format of date to insert with `insert-current-date-time' func
  See help of `format-time-string' for possible replacements")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
  Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-time-format (current-time))))

;;evil-leader config
(use-package evil-leader
    :init
    (add-to-list 'load-path "~/.emacs.d/packages/evil-leader")
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
	"<SPC>" 'helm-M-x
	"\\" 'magit-status
	"f"  'helm-find-files
	"t" 'insert-current-date-time
    ))

(use-package evil
    :init
    (setq evil-want-C-u-scroll t)
    :config
    (evil-mode 1)
    (define-key global-map (kbd "C-f") 'universal-argument)
    (define-key universal-argument-map (kbd "C-u") nil)
    (define-key universal-argument-map (kbd "C-f") 'universal-argument-more)
    (define-key global-map (kbd "C-u") 'kill-whole-line)
    (eval-after-load 'evil-maps
	'(progn
	    (define-key evil-motion-state-map (kbd "C-f") nil)
	    (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)))
    )

;; rebind <C-u> to intended behavior, otherwise defaults to universal-argument

(use-package evil-surround
    :config
    (global-evil-surround-mode 1))

(use-package evil-args
    :config
    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "i" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

    ;; bind evil-forward/backward-args
    (define-key evil-normal-state-map "L" 'evil-forward-arg)
    (define-key evil-normal-state-map "H" 'evil-backward-arg)
    (define-key evil-motion-state-map "L" 'evil-forward-arg)
    (define-key evil-motion-state-map "H" 'evil-backward-arg)

    ;; bind evil-jump-out-args
    (define-key evil-normal-state-map "K" 'evil-jump-out-args)
    )

(use-package evil-numbers
    :config
    (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
    )

;; evil number support
;;(require 'evil-numbers)
;;(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
;;(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

;; evil leader support
(use-package evil-commentary)
(use-package evil-replace-with-register)
(use-package evil-magit)

;; orgmode bindings

(use-package org-evil
    :config
    (setq org-M-RET-may-split-line nil) ;; so we can press 'o' in evil and generate the next item
    )

;; activate helm mode
(use-package helm
    :config
    (helm-mode 1)
    (global-set-key (kbd "M-x") 'helm-M-x)
    )

;;solarized dark theme
(use-package solarized-theme
    :config
    (setq solarized-use-variable-pitch nil
	    solarized-scale-org-headlines nil) ;;unscrew org layout
    (load-theme 'solarized-dark t)
    )

(use-package powershell)
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))
(use-package tramp-term)
(use-package evil-text-object-python)

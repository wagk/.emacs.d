(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "04886f04f33dfc156dc60b5a4b1f6f67da710eae8b39af6cf72f5f0b908948ee" default)))
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (projectile evil-avy evil-cleverparens evil-easymotion evil-escape evil-exchange evil-extra-operator evil-god-state evil-indent-textobject evil-mark-replace evil-matchit evil-mc evil-mc-extras evil-paredit evil-quickscope evil-smartparens evil-snipe evil-swap-keys evil-text-object-python evil-textobj-anyblock evil-textobj-column evil-visual-mark-mode evil-visualstar powerline-evil aws-ec2 evil-nerd-commenter 0blayout emms ercn web-mode ace-jump-mode evil-replace-with-register evil-numbers autotetris-mode dired+ evil-commentary evil-args evil-org evil-magit org-evil pomodoro solarized-theme syndicate chronos evil-surround evil helm)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
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
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
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

;; turn on line numbers
(global-linum-mode 1)

;; startup maximised
(add-to-list 'default-frame-alist '(fullscreen . maximised))

;;evil-leader config
(add-to-list 'load-path "~/.emacs.d/packages/evil-leader")
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-key
  "\\" 'helm-M-x
  "f"  'helm-find-files
  )

;; activate evil mode and associated plugins
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

(require 'evil-surround)
(global-evil-surround-mode 1)
;;(setq-default evil-surround-pairs-alist (cons '(?~ . ("~" . "~")) evil-surround-pairs-alist))

;; locate and load the package
(require 'evil-args)

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

;; evil number support
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

;; evil leader support
(require 'evil-commentary)
(require 'evil-replace-with-register)
(require 'evil-magit)

;; orgmode bindings
(require 'org-evil)
(setq org-M-RET-may-split-line nil) ;; so we can press 'o' in evil and generate the next item

;; activate helm mode
(require 'helm)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)

;;solarized dark theme
(setq solarized-use-variable-pitch nil
      solarized-scale-org-headlines nil) ;;unscrew org layout
(load-theme 'solarized-dark t)

;; set default font
(add-to-list 'default-frame-alist '(font . "Consolas-11"))
;;(set-frame-font "Consolas 11")

;;datetime things
(defvar current-date-time-format "%Y-%m-%dT%H:%M:%S"
    "Format of date to insert with `insert-current-date-time' func
    See help of `format-time-string' for possible replacements")

(defun insert-current-date-time ()
    "insert the current date and time into current buffer.
    Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       (insert (format-time-string current-date-time-format (current-time))))

(global-set-key (kbd "<f5>") 'insert-current-date-time)
;; datetime things


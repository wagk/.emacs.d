;; Solarized 1.0.0beta2[a] Color Palette[8]
;; | Color   |    |     |     | sRGB    |     |     |     | xterm | Terminal  | Usage                          |
;; |---------+----+-----+-----+---------+-----+-----+-----+-------+-----------+--------------------------------|
;; | Name    | L* | a*  | b*  | Hex     |   R |   G |   B |  Code | Name      |                                |
;; |---------+----+-----+-----+---------+-----+-----+-----+-------+-----------+--------------------------------|
;; | Base03  | 15 | −12 | −12 | #002b36 |   0 |  43 |  54 |   234 | brblack   | background tones (dark theme)  |
;; | Base02  | 20 | −12 | −12 | #073642 |   7 |  54 |  66 |   235 | black     | background tones (dark theme)  |
;; | Base01  | 45 | −07 | −07 | #586e75 |  88 | 110 | 117 |   240 | brgreen   | content tones                  |
;; | Base00  | 50 | −07 | −07 | #657b83 | 101 | 123 | 131 |   241 | bryellow  | content tones                  |
;; | Base0   | 60 | −06 | −03 | #839496 | 131 | 148 | 150 |   244 | brblue    | content tones                  |
;; | Base1   | 65 | −05 | −02 | #93a1a1 | 147 | 161 | 161 |   245 | brcyan    | content tones                  |
;; | Base2   | 92 | −00 | 10  | #eee8d5 | 238 | 232 | 213 |   254 | white     | background tones (light theme) |
;; | Base3   | 97 | 00  | 10  | #fdf6e3 | 253 | 246 | 227 |   230 | brwhite   | background tones (light theme) |
;; | Yellow  | 60 | 10  | 65  | #b58900 | 181 | 137 |   0 |   136 | yellow    | accent tones                   |
;; | Orange  | 50 | 50  | 55  | #cb4b16 | 203 |  75 |  22 |   166 | brred     | accent tones                   |
;; | Red     | 50 | 65  | 45  | #dc322f | 220 |  50 |  47 |   160 | red       | accent tones                   |
;; | Magenta | 50 | 65  | −05 | #d33682 | 211 |  54 | 130 |   125 | magenta   | accent tones                   |
;; | Violet  | 50 | 15  | −45 | #6c71c4 | 108 | 113 | 196 |    61 | brmagenta | accent tones                   |
;; | Blue    | 55 | −10 | −45 | #268bd2 |  38 | 139 | 210 |    33 | blue      | accent tones                   |
;; | Cyan    | 60 | −35 | −05 | #2aa198 |  42 | 161 | 152 |    37 | cyan      | accent tones                   |
;; | Green   | 60 | −20 | 65  | #859900 | 133 | 153 |   0 |    64 | green     | accent tones                   |

;; | SOLARIZED | HEX     | 16/8 | TERMCOL   | XTERM/HEX   | L*A*B      | RGB         | HSB         |
;; |-----------|---------|------|-----------|-------------|------------|-------------|-------------|
;; | base03    | #002b36 | 8/4  | brblack   | 234 #1c1c1c | 15 -12 -12 |   0  43  54 | 193 100  21 |
;; | base02    | #073642 | 0/4  | black     | 235 #262626 | 20 -12 -12 |   7  54  66 | 192  90  26 |
;; | base01    | #586e75 | 10/7 | brgreen   | 240 #585858 | 45 -07 -07 |  88 110 117 | 194  25  46 |
;; | base00    | #657b83 | 11/7 | bryello   | 241 #626262 | 50 -07 -07 | 101 123 131 | 195  23  51 |
;; | base0     | #839496 | 12/6 | brblue    | 244 #808080 | 60 -06 -03 | 131 148 150 | 186  13  59 |
;; | base1     | #93a1a1 | 14/4 | brcyan    | 245 #8a8a8a | 65 -05 -02 | 147 161 161 | 180   9  63 |
;; | base2     | #eee8d5 | 7/7  | white     | 254 #e4e4e4 | 92 -00  10 | 238 232 213 |  44  11  93 |
;; | base3     | #fdf6e3 | 15/7 | brwhite   | 230 #ffffd7 | 97  00  10 | 253 246 227 |  44  10  99 |
;; | yellow    | #b58900 | 3/3  | yellow    | 136 #af8700 | 60  10  65 | 181 137   0 |  45 100  71 |
;; | orange    | #cb4b16 | 9/3  | brred     | 166 #d75f00 | 50  50  55 | 203  75  22 |  18  89  80 |
;; | red       | #dc322f | 1/1  | red       | 160 #d70000 | 50  65  45 | 220  50  47 |   1  79  86 |
;; | magenta   | #d33682 | 5/5  | magenta   | 125 #af005f | 50  65 -05 | 211  54 130 | 331  74  83 |
;; | violet    | #6c71c4 | 13/5 | brmagenta | 61  #5f5faf | 50  15 -45 | 108 113 196 | 237  45  77 |
;; | blue      | #268bd2 | 4/4  | blue      | 33  #0087ff | 55 -10 -45 |  38 139 210 | 205  82  82 |
;; | cyan      | #2aa198 | 6/6  | cyan      | 37  #00afaf | 60 -35 -05 |  42 161 152 | 175  74  63 |
;; | green     | #859900 | 2/2  | green     | 64  #5f8700 | 60 -20  65 | 133 153   0 |  68 100  60 |

(dolist (col '((sol-base03  . "#002b36")
               (sol-base02  . "#073642")
               (sol-base01  . "#586e75")
               (sol-base00  . "#657b83")
               (sol-base0   . "#839496")
               (sol-base1   . "#93a1a1")
               (sol-base2   . "#eee8d5")
               (sol-base3   . "#fdf6e3")
               (sol-yellow  . "#b58900")
               (sol-orange  . "#cb4b16")
               (sol-red     . "#dc322f")
               (sol-magenta . "#d33682")
               (sol-violet  . "#6c71c4")
               (sol-blue    . "#268bd2")
               (sol-cyan    . "#2aa198")
               (sol-green   . "#859900")))
  ;; TODO: set documentation string
  ;; configure faces like this if and only if it's not already defined
  ;; in `local.el'
  (if (boundp (car col))
      (message "color %s is already bound to %s" (car col) (cdr col))
    (set (car col) (cdr col))))

(with-eval-after-load 'evil
  (evil-ex-define-cmd "faces" 'list-faces-display))

;; Fonts
(defcustom --default-font-size 20
  "Default frame font size."
  :group 'personal
  :type 'natnum
  :set
  #'(lambda (obj size)
      (set-default-toplevel-value obj size)
      (with-eval-after-load 'config-theme
        (--find-and-set-fonts))))

(cl-defun --find-and-set-fonts ()
  (interactive)
  (let ((sarasa-mono (font-spec :family "Sarasa Mono J" :size --default-font-size))
        (iosevka (font-spec :family "Iosevka" :size --default-font-size))
        (iosevka-etoile (font-spec :family "Iosevka Etoile" :size --default-font-size))
        (iosevka-aile (font-spec :family "Iosevka Aile" :size --default-font-size))
        (courier (font-spec :family "Courier" :size --default-font-size)))
    (cond
     ((find-font sarasa-mono)
      (set-frame-font sarasa-mono nil t)
      (custom-set-faces `(fixed-pitch ((default . (:font ,sarasa-mono))))
                        `(variable-pitch ((default . (:font ,sarasa-mono))))
                        `(fixed-pitch-serif ((default . (:font ,sarasa-mono))))))
     ((find-font iosevka)
      (set-frame-font iosevka nil t)
      (custom-set-faces `(fixed-pitch ((default . (:font ,iosevka))))
                        `(variable-pitch ((default . (:font ,iosevka))))
                        `(fixed-pitch-serif ((default . (:font ,iosevka)))))))
    ;; If iosevka proportional fonts are also found, use that.
    (when (find-font iosevka-etoile)
      (custom-set-faces `(variable-pitch ((default . (:font ,iosevka-etoile))))))
    (when (find-font iosevka-aile)
      (custom-set-faces `(fixed-pitch-serif ((default . (:font ,iosevka-aile))))))))

(add-hook 'after-init-hook #'--find-and-set-fonts)
(add-hook 'server-after-make-frame-hook #'--find-and-set-fonts)

(use-package solarized-definitions
  :if (display-graphic-p)
  :ensure (:host github :repo "sellout/emacs-color-theme-solarized" :main nil)
  :config
  (load-theme 'solarized t))

(customize-set-variable 'frame-background-mode 'nil)

(defface sol-background
  `((((background light)) . (:background ,sol-base3))
    (((background dark)) . (:background ,sol-base03)))
  "Default background coloring."
  :group 'personal)

(defface sol-background-i
  `((((background light)) . (:background ,sol-base01))
    (((background dark)) . (:background ,sol-base1)))
  "Inverted default background coloring."
  :group 'personal)

(defface sol-foreground
  `((((background light)) . (:foreground ,sol-base00))
    (((background dark)) . (:foreground ,sol-base0)))
  "Default foreground coloring."
  :group 'personal)

(defface sol-foreground-i
  `((default . (:inherit sol-background-i))
    (((background light)) . (:foreground ,sol-base3))
    (((background dark)) . (:foreground ,sol-base03)))
  "Inverted default foreground coloring."
  :group 'personal)

(defface sol-foreground-box
  `((((background light)) . (:box ,sol-base00))
    (((background dark)) . (:box ,sol-base0)))
  "Default foreground coloring. In a box."
  :group 'personal)

(defface sol-light-foreground
  `((((background light)) . (:foreground ,sol-base1))
    (((background dark)) . (:foreground ,sol-base01)))
  "Somewhat light foreground coloring"
  :group 'personal)

(defface sol-strong-foreground
  `((((background light)) . (:foreground ,sol-base02))
    (((background dark)) . (:foreground ,sol-base2)))
  "Darker foreground coloring"
  :group 'personal)

(defface sol-superstrong-foreground
  `((((background light)) . (:foreground ,sol-base03))
    (((background dark)) . (:foreground ,sol-base3)))
  "Darkest foreground coloring"
  :group 'personal)

(defface sol-light-background
  `((((background light)) . (:background ,sol-base1))
    (((background dark)) . (:background ,sol-base01)))
  "Somewhat light background coloring"
  :group 'personal)

(defface sol-light-background-i
  `((((background light)) . (:background ,sol-base01))
    (((background dark)) . (:background ,sol-base1)))
  "Somewhat light background coloring"
  :group 'personal)

(defface sol-superlight-foreground
  `((((background light)) . (:foreground ,sol-base2))
    (((background dark)) . (:foreground ,sol-base02)))
  "Very light foreground coloring"
  :group 'personal)

(defface sol-superlight-background
  `((((background light)) . (:background ,sol-base2))
    (((background dark)) . (:background ,sol-base02)))
  "Very light background coloring"
  :group 'personal)

(defface sol-superlight-background-i
  `((((background light)) . (:background ,sol-base02))
    (((background dark)) . (:background ,sol-base2)))
  "Very light background coloring"
  :group 'personal)

(defface sol-superlight-underline
  `((((background light)) . (:underline ,sol-base2))
    (((background dark)) . (:underline ,sol-base02)))
  "Very light underline coloring"
  :group 'personal)

(defface sol-light-underline
  `((((background light)) . (:underline ,sol-base1))
    (((background dark)) . (:underline ,sol-base01)))
  "Light underline coloring"
  :group 'personal)

(defface sol-superlight-overline
  `((((background light)) . (:overline ,sol-base2))
    (((background dark)) . (:overline ,sol-base02)))
  "Very light overline coloring"
  :group 'personal)

(defface sol-superlight-box
  `((((background light)) . (:box (:line-width (-1 . -1)
                                   :color ,sol-base2)))
    (((background dark)) . (:box (:line-width (-1 . -1)
                                  :color ,sol-base02))))
  "Very light box coloring"
  :group 'personal)

(defface sol-background-box
  `((((background light)) . (:box (:line-width (-1 . -1)
                                   :color ,sol-base3)))
    (((background dark)) . (:box (:line-width (-1 . -1)
                                  :color ,sol-base03))))
  "Background box coloring"
  :group 'personal)

(defface sol-info
  `((default . (:italic t
                :inherit sol-light-foreground)))
  "Global default for notes."
  :group 'personal)

(defface sol-warning
  `((default . (:italic t
                :foreground ,sol-orange
                :inherit (sol-strong-foreground))))
  "Global default for warnings."
  :group 'personal)

(defface sol-error
  `((default . (:italic t
                :foreground ,sol-red
                :inherit (sol-strong-foreground))))
  "Global default for errors."
  :group 'personal)

;; Replaces the default arrows you see in the left and right fringe
;; with a nicer-looking bitmap. There's no really good place to put
;; this configuration (`fringe-indicator-alist' is a C variable), so
;; put it here.
(setf (alist-get 'continuation fringe-indicator-alist) 'empty-line)
(setf (alist-get 'truncation fringe-indicator-alist) 'empty-line)

(provide 'config-theme)

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
               (sol-base2   . "white")
               (sol-base3   . "brwhite")
               (sol-yellow  . "#b58900")
               (sol-orange  . "#cb4b16")
               (sol-red     . "#dc322f")
               (sol-magenta . "#d33682")
               (sol-violet  . "#6c71c4")
               (sol-blue    . "#268bd2")
               (sol-cyan    . "#2aa198")
               (sol-green   . "#859900")))
  ;; TODO: set documentation string
  (set (car col) (cdr col)))

(setq frame-background-mode 'light)

(remove-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; (global-display-fill-column-indicator-mode -1)

(use-package solarized-definitions
  :ensure (:host github :repo "sellout/emacs-color-theme-solarized" :main nil)
  :config
  (load-theme 'solarized t))

;; (load-theme 'modus-vivendi t)
;; (load-theme 'solarized t)

(with-eval-after-load 'faces
  (set-face-attribute 'mode-line-inactive nil
                      :box '(:line-width 3)
                      :foreground 'unspecified
                      :background 'unspecified
                      :inverse-video nil
                      :inherit 'default)
  (set-face-attribute 'mode-line-active nil
                      :box '(:line-width 3)
                      :foreground 'unspecified
                      :background 'unspecified
                      :inverse-video nil
                      :inherit '(default highlight)))

(with-eval-after-load 'dired-filter
  (set-face-attribute 'dired-filter-group-header nil
                      :background 'unspecified
                      :foreground 'unspecified
                      :inherit 'default))

(with-eval-after-load 'tab-bar
  (set-face-attribute 'tab-bar nil
                      :foreground))

(provide 'config-theme-term)

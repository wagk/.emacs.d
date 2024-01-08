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
  (set (car col) (cdr col)))

(defface sol-subtle `((((background light)) (:foreground ,sol-base1))
                      (((background dark)) (:foreground ,sol-base01)))
  "DEPRECATED: Some parts of the config uses this, so we can't remove it yet.")

(defface sol-subtle-background `((((background light)) (:foreground ,sol-base1 :background ,sol-base2))
                                 (((background dark)) (:foreground ,sol-base01 :background ,sol-base02)))
  "DEPRECATED: Some parts of the config uses this, so we can't remove it yet.")

(use-package nano-theme
  :straight t
  :custom
  (nano-window-divider-show t)
  (nano-fonts-use nil)) ;; for now

(customize-set-variable 'nano-light-foreground sol-base00)
(customize-set-variable 'nano-light-background sol-base3)
(customize-set-variable 'nano-light-highlight  sol-base2)
(customize-set-variable 'nano-light-subtle     sol-base1)
(customize-set-variable 'nano-light-faded      sol-base0)
(customize-set-variable 'nano-light-salient    sol-base01)
(customize-set-variable 'nano-light-strong     sol-yellow)
(customize-set-variable 'nano-light-popout     sol-orange)
(customize-set-variable 'nano-light-critical   sol-red)
(customize-set-variable 'nano-dark-foreground  sol-base0)
(customize-set-variable 'nano-dark-background  sol-base03)
(customize-set-variable 'nano-dark-highlight   sol-base02)
(customize-set-variable 'nano-dark-subtle      sol-base01)
(customize-set-variable 'nano-dark-faded       sol-base00)
(customize-set-variable 'nano-dark-salient     sol-base1)
(customize-set-variable 'nano-dark-strong      sol-yellow)
(customize-set-variable 'nano-dark-popout      sol-orange)
(customize-set-variable 'nano-dark-critical    sol-red)

(provide 'config-theme)

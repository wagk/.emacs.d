
(defun my-init-solarized-color-variables-and-other-font-things ()
  "Solarized 1.0.0beta2[a] Color Palette[8]
 | Color   |    |     |     | sRGB    |     |     |     | xterm | Terminal  | Usage                          |
 |---------+----+-----+-----+---------+-----+-----+-----+-------+-----------+--------------------------------|
 | Name    | L* | a*  | b*  | Hex     |   R |   G |   B |  Code | Name      |                                |
 |---------+----+-----+-----+---------+-----+-----+-----+-------+-----------+--------------------------------|
 | Base03  | 15 | −12 | −12 | #002b36 |   0 |  43 |  54 |   234 | brblack   | background tones (dark theme)  |
 | Base02  | 20 | −12 | −12 | #073642 |   7 |  54 |  66 |   235 | black     | background tones (dark theme)  |
 | Base01  | 45 | −07 | −07 | #586e75 |  88 | 110 | 117 |   240 | brgreen   | content tones                  |
 | Base00  | 50 | −07 | −07 | #657b83 | 101 | 123 | 131 |   241 | bryellow  | content tones                  |
 | Base0   | 60 | −06 | −03 | #839496 | 131 | 148 | 150 |   244 | brblue    | content tones                  |
 | Base1   | 65 | −05 | −02 | #93a1a1 | 147 | 161 | 161 |   245 | brcyan    | content tones                  |
 | Base2   | 92 | −00 | 10  | #eee8d5 | 238 | 232 | 213 |   254 | white     | background tones (light theme) |
 | Base3   | 97 | 00  | 10  | #fdf6e3 | 253 | 246 | 227 |   230 | brwhite   | background tones (light theme) |
 | Yellow  | 60 | 10  | 65  | #b58900 | 181 | 137 |   0 |   136 | yellow    | accent tones                   |
 | Orange  | 50 | 50  | 55  | #cb4b16 | 203 |  75 |  22 |   166 | brred     | accent tones                   |
 | Red     | 50 | 65  | 45  | #dc322f | 220 |  50 |  47 |   160 | red       | accent tones                   |
 | Magenta | 50 | 65  | −05 | #d33682 | 211 |  54 | 130 |   125 | magenta   | accent tones                   |
 | Violet  | 50 | 15  | −45 | #6c71c4 | 108 | 113 | 196 |    61 | brmagenta | accent tones                   |
 | Blue    | 55 | −10 | −45 | #268bd2 |  38 | 139 | 210 |    33 | blue      | accent tones                   |
 | Cyan    | 60 | −35 | −05 | #2aa198 |  42 | 161 | 152 |    37 | cyan      | accent tones                   |
 | Green   | 60 | −20 | 65  | #859900 | 133 | 153 |   0 |    64 | green     | accent tones                   |"
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

  ;; TODO: work on this
  (defface sol-critical '((t (:inherit default)))
    "Follows `nano-critical', for information that requires immediate action.
   It should be of high constrast when compared to other faces. This can
   be realized (for example) by setting an intense background color,
   typically a shade of red. It must be used scarcely.")

  (defface sol-popout '((t (:inherit default)))
    "Follows `nano-popout', for information that needs attention.
   To achieve such effect, the hue of the face has to be sufficiently
   different from other faces such that it attracts attention through the
   popout effect.")

  (defface sol-strong '((t (:inherit default)))
    "Follows `nano-strong', for information of a structural nature.
   It has to be the same color as the default color and only the weight
   differs by one level (e.g., light/regular or regular/bold). It is
   generally used for titles, keywords, directory, etc.")

  (defface sol-salient '((t (:inherit default)))
    "Follows `nano-salient', information that are important.
   To suggest the information is of the same nature but important, the
   face uses a different hue with approximately the same intensity as the
   default face. This is typically used for links.")

  (defface sol-faded '((t (:inherit default)))
    "Follows `nano-faded', Faded face is for information that are less important.
   It is made by using the same hue as the default but with a lesser
   intensity than the default. It can be used for comments, secondary
   information and also replace italic (which is generally abused
                                        anyway).")

  (defface sol-subtle `((((background light)) (:foreground ,sol-base1))
                        (((background dark)) (:foreground ,sol-base01)))
    "Follows `nano-subtle', to suggest a physical area on the screen.
   It is important to not disturb too strongly the reading of information
   and this can be made by setting a very light background color that is
   barely perceptible.")

  (defface sol-default '((t (:inherit default)))
    "Follows `nano-default'"))

(my-init-solarized-color-variables-and-other-font-things)

(provide 'config-colors)

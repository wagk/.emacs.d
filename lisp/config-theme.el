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

;; Nano theme

(use-package nano-theme
  :straight t
  :custom
  (nano-window-divider-show t)
  (nano-fonts-use nil)) ;; for now

(customize-set-variable 'nano-light-foreground sol-base00)
(customize-set-variable 'nano-light-background sol-base3)
(customize-set-variable 'nano-light-highlight  sol-base2)
(customize-set-variable 'nano-light-subtle     sol-base2)
(customize-set-variable 'nano-light-faded      sol-base0)
(customize-set-variable 'nano-light-salient    sol-green)
(customize-set-variable 'nano-light-strong     sol-base01)
(customize-set-variable 'nano-light-popout     sol-orange)
(customize-set-variable 'nano-light-critical   sol-red)

(customize-set-variable 'nano-dark-foreground  sol-base0)
(customize-set-variable 'nano-dark-background  sol-base03)
(customize-set-variable 'nano-dark-highlight   sol-base02)
(customize-set-variable 'nano-dark-subtle      sol-base02)
(customize-set-variable 'nano-dark-faded       sol-base00)
(customize-set-variable 'nano-dark-salient     sol-cyan)
(customize-set-variable 'nano-dark-strong      sol-base1)
(customize-set-variable 'nano-dark-popout      sol-magenta)
(customize-set-variable 'nano-dark-critical    sol-red)

(load-theme 'nano-light t)

(defface sol-superlight-foreground
  `((((background light)) (:foreground ,sol-base2))
    (((background dark)) (:foreground ,sol-base02)))
  "Very light foreground coloring"
  :group 'personal)

(defface sol-superlight-background
  `((((background light)) (:background ,sol-base2))
    (((background dark)) (:background ,sol-base02)))
  "Very light background coloring"
  :group 'personal)

(with-eval-after-load 'faces
  (set-face-attribute 'fill-column-indicator nil
                      :inherit 'sol-superlight-foreground)
  (set-face-attribute 'show-paren-match nil
                      :foreground 'unspecified))

(with-eval-after-load 'flycheck
  (custom-set-faces `(flycheck-error ((((background light))
                                       (:underline ,sol-red))
                                      (((background dark))
                                       (:underline ,sol-red))))
                    `(flycheck-info ((((background light))
                                      (:underline ,sol-base2))
                                     (((background dark))
                                      (:underline ,sol-base02))))
                    `(flycheck-warning ((((background light))
                                         (:underline ,sol-base1))
                                        (((background dark))
                                         (:underline ,sol-base01))))))

(with-eval-after-load 'magit
  (set-face-attribute 'magit-diff-hunk-heading nil
                      :bold t
                      :inherit 'nano-faded))

(with-eval-after-load 'markdown-mode
  (set-face-attribute 'markdown-inline-code-face nil
                      :inherit 'nano-subtle)
  (set-face-attribute 'markdown-italic-face nil
                      :italic t)
  (set-face-attribute 'markdown-url-face nil
                      :inherit 'markdown-plain-url-face)

  (defface --markdown-tag-face
    '((t (:bold t :inherit nano-faded)))
    "Face used to describe tags (like `#foo'). I like using tags."
    :group 'personal)
  (defconst --markdown-tag-keyword-regex
    (rx (or line-start space) "#" (one-or-more (any alnum "_" "-"))))
  (font-lock-add-keywords
   'markdown-mode `((,--markdown-tag-keyword-regex 0 '--markdown-tag-face))))

(with-eval-after-load 'isearch
  (set-face-attribute 'lazy-highlight nil
                      :inherit 'match))

;; font-lock
(set-face-attribute 'font-lock-keyword-face nil
                    :foreground 'unspecified
                    :inherit 'nano-default)
(set-face-attribute 'font-lock-constant-face nil
                    :foreground 'unspecified
                    :inherit 'nano-default)
(set-face-attribute 'font-lock-function-name-face nil
                    :foreground 'unspecified
                    :inherit 'nano-default)
(set-face-attribute 'font-lock-builtin-face nil
                    :foreground 'unspecified
                    :inherit 'nano-default)
(set-face-attribute 'font-lock-variable-name-face nil
                    :foreground 'unspecified
                    :inherit 'nano-strong)
(set-face-attribute 'font-lock-comment-face nil
                    :foreground 'unspecified
                    :inherit '(nano-faded sol-superlight-background))
(set-face-attribute 'font-lock-doc-face nil
                    :foreground 'unspecified
                    :inherit '(nano-default sol-superlight-background))
(set-face-attribute 'font-lock-type-face nil
                    :italic t
                    :foreground 'unspecified
                    :inherit 'nano-default)
(set-face-attribute 'font-lock-string-face nil
                    :foreground 'unspecified)

(with-eval-after-load 'tree-sitter-hl
  (set-face-attribute 'tree-sitter-hl-face:function nil
                      :inherit 'nano-default)
  (set-face-attribute 'tree-sitter-hl-face:function.call nil
                      :inherit 'tree-sitter-hl-face:function)
  (set-face-attribute 'tree-sitter-hl-face:function.builtin nil
                      :inherit 'tree-sitter-hl-face:function)

  (set-face-attribute 'tree-sitter-hl-face:method nil
                      :inherit 'tree-sitter-hl-face:function)
  (set-face-attribute 'tree-sitter-hl-face:method.call nil
                      :inherit 'tree-sitter-hl-face:method)

  (set-face-attribute 'tree-sitter-hl-face:variable.builtin nil
                      :inherit 'tree-sitter-hl-face:variable)
  (set-face-attribute 'tree-sitter-hl-face:variable.special nil
                      :bold t
                      :inherit 'font-lock-warning-face)

  (set-face-attribute 'tree-sitter-hl-face:attribute nil
                      :inherit 'nano-default)

  (set-face-attribute 'tree-sitter-hl-face:type nil
                      :inherit 'font-lock-type-face)
  (set-face-attribute 'tree-sitter-hl-face:type.argument nil
                      :inherit 'tree-sitter-hl-face:type)
  (set-face-attribute 'tree-sitter-hl-face:type.builtin nil
                      :inherit 'tree-sitter-hl-face:type)
  (set-face-attribute 'tree-sitter-hl-face:type.super nil
                      :inherit 'tree-sitter-hl-face:type)

  (set-face-attribute 'tree-sitter-hl-face:function.macro nil
                      :inherit 'nano-salient)

  (set-face-attribute 'tree-sitter-hl-face:property nil
                      :italic nil
                      :inherit 'font-lock-function-name-face))

(with-eval-after-load 'term
 (set-face-attribute 'term-color-red nil
                     :foreground sol-red
                     :background sol-red)
 (set-face-attribute 'term-color-green nil
                     :foreground sol-green
                     :background sol-green)
 (set-face-attribute 'term-color-yellow nil
                     :foreground sol-yellow
                     :background sol-yellow)
 (set-face-attribute 'term-color-blue nil
                     :foreground sol-blue
                     :background sol-blue)
 (set-face-attribute 'term-color-magenta nil
                     :foreground sol-magenta
                     :background sol-magenta)
 (set-face-attribute 'term-color-cyan nil
                     :foreground sol-cyan
                     :background sol-cyan)
 (set-face-attribute 'term-color-white nil
                     :foreground sol-base2
                     :background sol-base2))

(with-eval-after-load 'smerge-mode
  (set-face-attribute 'smerge-base nil
                      :foreground sol-blue
                      :background 'unspecified)
  (set-face-attribute 'smerge-lower nil
                      :foreground sol-green
                      :background 'unspecified)
  (set-face-attribute 'smerge-upper nil
                      :foreground sol-red
                      :background 'unspecified))

;; Solarized theme

(use-package solarized-theme
  :disabled t
  :straight (:host github :repo "bbatsov/solarized-emacs")
  ;; :if (display-graphic-p)
  :custom
  (solarized-use-variable-pitch nil)
  (solarized-distinct-fringe-background nil)
  (solarized-high-contrast-mode-line nil)
  (solarized-use-less-bold t)
  (solarized-use-more-italic nil)
  (solarized-scale-org-headlines nil)
  (solarized-height-minus-1 1.0)
  (solarized-height-plus-1 1.0)
  (solarized-height-plus-2 1.0)
  (solarized-height-plus-3 1.0)
  (solarized-height-plus-4 1.0))

(provide 'config-theme)
